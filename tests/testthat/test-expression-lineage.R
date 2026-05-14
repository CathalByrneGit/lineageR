test_that("lin_record_expression inserts rows into cell_expression_lineage", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  n <- DBI::dbGetQuery(h$ctx$con,
    "SELECT COUNT(*) AS n FROM cell_expression_lineage")$n
  expect_true(n >= 2L)
})

test_that("lin_record_expression upserts on the same output/column/transform triple", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  lin_record_expression(h$ctx,
    transform_node_id = h$enc_tform_id,
    output_node_id    = h$enc_ds_id,
    output_column     = "los_days",
    source_node_ids   = h$beds_id,
    source_columns    = "los_days",
    expression_text   = "MAX(bed_stays.los_days) FILTER (WHERE stay_type = 'acute')"
  )

  rows <- DBI::dbGetQuery(h$ctx$con,
    "SELECT expression_text FROM cell_expression_lineage
     WHERE output_node_id = ? AND output_column = ?",
    params = list(h$enc_ds_id, "los_days")
  )
  expect_equal(nrow(rows), 1L)
  expect_match(rows$expression_text, "FILTER")
})

test_that("lin_column_provenance returns a data frame with expected columns", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  cp <- lin_column_provenance(h$ctx, h$enc_ds_id)
  expect_s3_class(cp, "data.frame")
  expect_true(all(c("output_column", "transform_name",
                    "expression_text", "source_node_name",
                    "source_columns") %in% names(cp)))
  expect_true(nrow(cp) >= 2L)
})

test_that("lin_column_provenance filters by column name", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  cp <- lin_column_provenance(h$ctx, h$enc_ds_id, column = "los_days")
  expect_true(nrow(cp) >= 1L)
  expect_true(all(cp$output_column == "los_days"))
})

test_that("lin_column_provenance returns empty frame for unknown node", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  cp <- lin_column_provenance(h$ctx, "no-such-node")
  expect_equal(nrow(cp), 0L)
  expect_true("output_column" %in% names(cp))
})

test_that("lin_wrap_sql returns the original SQL unchanged", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  sql    <- "SELECT los_days, diagnosis_complete FROM encounters"
  result <- lin_wrap_sql(h$ctx,
    transform_node_id = h$enc_tform_id,
    sql               = sql,
    input_node_ids    = h$enc_ds_id,
    output_node_id    = h$enc_ds_id
  )
  expect_equal(result, sql)
})

test_that("lin_wrap_sql captures expression lineage for each output column", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  # fresh transform + dataset to avoid conflicts with fixture data
  tform2 <- lin_register_transform(h$ctx, "wrap_test_tform")
  ds2    <- lin_register_dataset(h$ctx,  "wrap_test_ds", table_name = "encounters")
  lin_add_input(h$ctx, tform2, h$enc_ds_id)
  lin_add_output(h$ctx, tform2, ds2)

  lin_wrap_sql(h$ctx,
    transform_node_id = tform2,
    sql               = "SELECT los_days AS los_days, diagnosis_complete AS diag FROM encounters",
    input_node_ids    = h$enc_ds_id,
    output_node_id    = ds2
  )

  n <- DBI::dbGetQuery(h$ctx$con,
    "SELECT COUNT(*) AS n FROM cell_expression_lineage
     WHERE output_node_id = ?",
    params = list(ds2))$n
  expect_true(n >= 1L)
})

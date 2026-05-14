test_that("lin_record_row_provenance inserts rows into row_provenance", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  n <- DBI::dbGetQuery(h$ctx$con,
    "SELECT COUNT(*) AS n FROM row_provenance")$n
  expect_true(n >= 2L)
})

test_that("lin_row_provenance depth=1 returns source rows for E-123", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  prov <- lin_row_provenance(h$ctx, h$enc_ds_id, "E-123", depth = 1L)
  expect_true(nrow(prov) >= 1L)
  expect_true(all(c("source_node_id", "source_pk_value", "depth") %in% names(prov)))
  expect_true("stay_789" %in% prov$source_pk_value ||
              "dx_456"   %in% prov$source_pk_value)
})

test_that("lin_row_provenance returns empty frame for unknown pk_value", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  prov <- lin_row_provenance(h$ctx, h$enc_ds_id, "E-UNKNOWN")
  expect_equal(nrow(prov), 0L)
  expect_true("source_node_id" %in% names(prov))
})

test_that("lin_row_provenance depth column matches the hop number", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  prov <- lin_row_provenance(h$ctx, h$enc_ds_id, "E-123", depth = 1L)
  expect_true(all(prov$depth == 1L))
})

test_that("lin_impact_rows finds output rows derived from stay_789", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  impact <- lin_impact_rows(h$ctx, h$beds_id, "stay_789")
  expect_true(nrow(impact) >= 1L)
  expect_true("E-123" %in% impact$output_pk_value)
  expect_true(all(c("output_node_id", "output_node_name",
                    "output_pk_value") %in% names(impact)))
})

test_that("lin_impact_rows returns empty frame for unknown source pk", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  impact <- lin_impact_rows(h$ctx, h$beds_id, "stay_UNKNOWN")
  expect_equal(nrow(impact), 0L)
  expect_true("output_node_id" %in% names(impact))
})

test_that("lin_record_row_provenance samples when mapping exceeds 100k rows", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  big_map <- data.frame(
    output_pk_value = as.character(seq_len(110000L)),
    source_pk_value = as.character(seq_len(110000L)),
    stringsAsFactors = FALSE
  )
  n_inserted <- lin_record_row_provenance(h$ctx,
    output_node_id  = h$enc_ds_id,
    output_pk_col   = "enc_id",
    source_node_id  = h$beds_id,
    source_pk_col   = "stay_id",
    mapping         = big_map
  )
  expect_equal(n_inserted, 10000L)
})

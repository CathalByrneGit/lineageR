test_that("lin_register_source creates a source node with correct fields", {
  ctx <- make_ctx()
  id  <- lin_register_source(ctx, "s3_raw",
                              description       = "Raw S3 data",
                              connection_string = "s3://bucket/raw/")

  row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_nodes WHERE node_id = ?", params = list(id))

  expect_equal(nrow(row), 1L)
  expect_equal(row$node_type, "source")
  expect_equal(row$name,      "s3_raw")
  expect_equal(row$description, "Raw S3 data")
  expect_true(grepl("s3://bucket/raw/", row$metadata_json))
})

test_that("lin_register_transform stores language and script_path in metadata", {
  ctx <- make_ctx()
  id  <- lin_register_transform(ctx, "clean_airports",
                                 script_path = "R/clean.R",
                                 language    = "r")

  row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_nodes WHERE node_id = ?", params = list(id))

  expect_equal(row$node_type, "transform")
  expect_true(grepl("clean.R", row$metadata_json))
  expect_true(grepl('"r"',     row$metadata_json))
})

test_that("lin_register_dataset stores table_name in metadata", {
  ctx <- make_ctx()
  id  <- lin_register_dataset(ctx, "airports_clean", table_name = "airports_clean")

  row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_nodes WHERE node_id = ?", params = list(id))

  expect_equal(row$node_type, "dataset")
  expect_true(grepl("airports_clean", row$metadata_json))
})

test_that("lin_register_object_type is idempotent", {
  ctx <- make_ctx()
  id1 <- lin_register_object_type(ctx, "Airport")
  id2 <- lin_register_object_type(ctx, "Airport")

  expect_equal(id1, id2)

  rows <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_nodes WHERE name = 'Airport' AND node_type = 'object_type'")
  expect_equal(nrow(rows), 1L)
})

test_that("lin_sync_bundle creates one node per object type", {
  ctx    <- make_ctx()
  bundle <- list(
    object_types = list(
      Airport     = list(id = "Airport"),
      FlightRoute = list(id = "FlightRoute")
    )
  )
  lin_sync_bundle(ctx, bundle)

  rows <- DBI::dbGetQuery(ctx$con,
    "SELECT name FROM lineage_nodes WHERE node_type = 'object_type'
     ORDER BY name")
  expect_equal(rows$name, c("Airport", "FlightRoute"))
})

test_that("lin_sync_bundle is idempotent on second call", {
  ctx    <- make_ctx()
  bundle <- list(
    object_types = list(
      Airport     = list(id = "Airport"),
      FlightRoute = list(id = "FlightRoute")
    )
  )
  ids1 <- lin_sync_bundle(ctx, bundle)
  ids2 <- lin_sync_bundle(ctx, bundle)

  expect_equal(sort(ids1), sort(ids2))

  rows <- DBI::dbGetQuery(ctx$con,
    "SELECT COUNT(*) AS n FROM lineage_nodes WHERE node_type = 'object_type'")
  expect_equal(rows$n, 2L)
})

test_that("lin_sync_bundle wires backing edge via lineage extension", {
  ctx      <- make_ctx()
  ds_id    <- lin_register_dataset(ctx, "airports_ds")
  bundle   <- list(
    object_types = list(
      Airport = list(id = "Airport",
                     extensions = list(lineage = list(dataset_node_id = ds_id)))
    )
  )
  lin_sync_bundle(ctx, bundle)

  ot_row <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id FROM lineage_nodes WHERE name = 'Airport'")
  edge_row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_edges WHERE from_node_id = ? AND to_node_id = ?",
    params = list(ds_id, ot_row$node_id))
  expect_equal(nrow(edge_row), 1L)
  expect_equal(edge_row$edge_type, "backs")
})

test_that("schema initialisation is idempotent", {
  ctx <- make_ctx()
  # Calling lineage_context again on same connection should not error
  expect_no_error(lineage_context(ctx$con))
})

test_that("lin_provenance(Airport) returns all 7 upstream nodes", {
  f   <- build_fixture()
  prov <- lin_provenance(f$ctx, f$airport_id)

  expect_equal(nrow(prov), 7L)
  expect_setequal(prov$name,
    c("s3_bucket", "ingest", "raw", "clean", "clean_a", "join", "joined"))
})

test_that("lin_provenance(Airport, depth=1) returns only the joined dataset", {
  f    <- build_fixture()
  prov <- lin_provenance(f$ctx, f$airport_id, depth = 1L)

  expect_equal(nrow(prov), 1L)
  expect_equal(prov$name, "joined")
  expect_equal(prov$distance, 1L)
})

test_that("lin_provenance returns empty data frame for isolated node", {
  f    <- build_fixture()
  prov <- lin_provenance(f$ctx, f$pg_id)   # postgres has no incoming edges

  expect_equal(nrow(prov), 0L)
})

test_that("lin_impact(S3) returns all 7 downstream nodes", {
  f      <- build_fixture()
  impact <- lin_impact(f$ctx, f$s3_id)

  expect_equal(nrow(impact), 7L)
  expect_setequal(impact$name,
    c("ingest", "raw", "clean", "clean_a", "join", "joined", "Airport"))
})

test_that("lin_impact returns empty data frame for isolated node", {
  f      <- build_fixture()
  impact <- lin_impact(f$ctx, f$pg_id)   # postgres has no outgoing chain

  expect_equal(nrow(impact), 0L)
})

test_that("lin_path(S3, Airport) returns the 8-node path in order", {
  f    <- build_fixture()
  path <- lin_path(f$ctx, f$s3_id, f$airport_id)

  expect_equal(nrow(path), 8L)
  expect_equal(path$name,
    c("s3_bucket", "ingest", "raw", "clean", "clean_a", "join", "joined", "Airport"))
  expect_equal(path$order, 1:8)
})

test_that("lin_path returns NULL when no path exists", {
  f    <- build_fixture()
  path <- lin_path(f$ctx, f$airport_id, f$s3_id)   # reverse direction

  expect_null(path)
})

test_that("lin_graph returns an igraph with correct vertex/edge counts", {
  f <- build_fixture()
  g <- lin_graph(f$ctx)

  expect_equal(igraph::vcount(g), 11L)   # 2 src + 3 trans + 4 ds + 2 ot
  expect_equal(igraph::ecount(g),  7L)
})

test_that("lin_stale identifies stale nodes based on last_updated_at", {
  f <- build_fixture()

  old_time <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC")
  new_time <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")

  # S3 updated recently
  DBI::dbExecute(f$ctx$con,
    "UPDATE lineage_nodes SET last_updated_at = ? WHERE node_id = ?",
    params = list(new_time, f$s3_id))

  # join transform and Airport are stamped with an older time
  DBI::dbExecute(f$ctx$con,
    "UPDATE lineage_nodes SET last_updated_at = ? WHERE node_id = ?",
    params = list(old_time, f$join_id))
  DBI::dbExecute(f$ctx$con,
    "UPDATE lineage_nodes SET last_updated_at = ? WHERE node_id = ?",
    params = list(old_time, f$airport_id))

  stale <- lin_stale(f$ctx)

  expect_true("join"    %in% stale$name)
  expect_true("Airport" %in% stale$name)
  expect_equal(nrow(stale), 2L)
})

test_that("lin_stale returns empty data frame when nothing is stale", {
  f      <- build_fixture()
  stale  <- lin_stale(f$ctx)   # no timestamps set at all

  expect_equal(nrow(stale), 0L)
})

test_that("lin_graph stale vertex attribute is correct", {
  f <- build_fixture()

  old_time <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC")
  new_time <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")

  DBI::dbExecute(f$ctx$con,
    "UPDATE lineage_nodes SET last_updated_at = ? WHERE node_id = ?",
    params = list(new_time, f$s3_id))
  DBI::dbExecute(f$ctx$con,
    "UPDATE lineage_nodes SET last_updated_at = ? WHERE node_id = ?",
    params = list(old_time, f$airport_id))

  g     <- lin_graph(f$ctx)
  v_s3  <- igraph::V(g)[igraph::V(g)$name == f$s3_id]
  v_air <- igraph::V(g)[igraph::V(g)$name == f$airport_id]

  expect_false(v_s3$stale)
  expect_true(v_air$stale)
})

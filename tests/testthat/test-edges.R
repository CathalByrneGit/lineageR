test_that("lin_add_input adds a consumes edge from input to transform", {
  ctx   <- make_ctx()
  src   <- lin_register_source(ctx,    "src")
  tform <- lin_register_transform(ctx, "tform")

  lin_add_input(ctx, tform, src)

  edge <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_edges WHERE from_node_id = ? AND to_node_id = ?",
    params = list(src, tform))
  expect_equal(nrow(edge), 1L)
  expect_equal(edge$edge_type, "consumes")
})

test_that("lin_add_output adds a produces edge from transform to dataset", {
  ctx   <- make_ctx()
  tform <- lin_register_transform(ctx, "tform")
  ds    <- lin_register_dataset(ctx,   "ds")

  lin_add_output(ctx, tform, ds)

  edge <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_edges WHERE from_node_id = ? AND to_node_id = ?",
    params = list(tform, ds))
  expect_equal(nrow(edge), 1L)
  expect_equal(edge$edge_type, "produces")
})

test_that("lin_add_backing adds a backs edge from dataset to object type", {
  ctx  <- make_ctx()
  ds   <- lin_register_dataset(ctx,     "ds")
  ot   <- lin_register_object_type(ctx, "Airport")

  lin_add_backing(ctx, ds, ot)

  edge <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_edges WHERE from_node_id = ? AND to_node_id = ?",
    params = list(ds, ot))
  expect_equal(nrow(edge), 1L)
  expect_equal(edge$edge_type, "backs")
})

test_that("duplicate edges are silently ignored (idempotent)", {
  ctx   <- make_ctx()
  src   <- lin_register_source(ctx,    "src")
  tform <- lin_register_transform(ctx, "tform")

  lin_add_input(ctx, tform, src)
  lin_add_input(ctx, tform, src)   # second call

  edges <- DBI::dbGetQuery(ctx$con, "SELECT COUNT(*) AS n FROM lineage_edges")
  expect_equal(edges$n, 1L)
})

test_that("adding an edge referencing a missing node aborts with error", {
  ctx <- make_ctx()
  ds  <- lin_register_dataset(ctx, "ds")

  expect_error(lin_add_input(ctx, "nonexistent_id", ds), class = "rlang_error")
})

test_that("adding an edge that creates a direct cycle aborts with error", {
  ctx   <- make_ctx()
  ds_a  <- lin_register_dataset(ctx,   "A")
  tform <- lin_register_transform(ctx, "T")
  ds_b  <- lin_register_dataset(ctx,   "B")

  lin_add_input(ctx,  tform, ds_a)   # A -> T
  lin_add_output(ctx, tform, ds_b)   # T -> B

  # Attempt to add B -> A (would form cycle B -> A -> T -> B)
  expect_error(
    lin_add_input(ctx, ds_a, ds_b),  # ds_b -> ds_a
    regexp = "[Cc]ycle"
  )
})

test_that("adding an edge that creates a longer cycle aborts with error", {
  ctx    <- make_ctx()
  n1     <- lin_register_dataset(ctx,   "N1")
  n2     <- lin_register_transform(ctx, "N2")
  n3     <- lin_register_dataset(ctx,   "N3")
  n4     <- lin_register_transform(ctx, "N4")

  lin_add_input(ctx,  n2, n1)   # N1 -> N2
  lin_add_output(ctx, n2, n3)   # N2 -> N3
  lin_add_input(ctx,  n4, n3)   # N3 -> N4

  # N4 -> N1 would close the cycle
  expect_error(
    lin_add_input(ctx, n1, n4),
    regexp = "[Cc]ycle"
  )
})

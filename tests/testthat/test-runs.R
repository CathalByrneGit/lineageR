test_that("lin_run_start inserts a running row in the run log", {
  ctx   <- make_ctx()
  tform <- lin_register_transform(ctx, "t1")

  run_id <- lin_run_start(ctx, tform)

  row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_run_log WHERE run_id = ?", params = list(run_id))
  expect_equal(nrow(row), 1L)
  expect_equal(row$status,  "running")
  expect_equal(row$node_id, tform)
  expect_true(is.na(row$completed_at))
})

test_that("lin_run_complete sets status = success and updates output last_updated_at", {
  ctx   <- make_ctx()
  tform <- lin_register_transform(ctx, "t1")
  ds    <- lin_register_dataset(ctx,   "output_ds")
  lin_add_output(ctx, tform, ds)

  run_id <- lin_run_start(ctx, tform)
  Sys.sleep(0.01)   # ensure completed_at > started_at
  lin_run_complete(ctx, run_id, rows_produced = 500L)

  log_row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_run_log WHERE run_id = ?", params = list(run_id))
  expect_equal(log_row$status,        "success")
  expect_equal(log_row$rows_produced, 500L)
  expect_false(is.na(log_row$completed_at))

  ds_row <- DBI::dbGetQuery(ctx$con,
    "SELECT last_updated_at FROM lineage_nodes WHERE node_id = ?",
    params = list(ds))
  expect_false(is.na(ds_row$last_updated_at))
})

test_that("lin_run_complete updates last_updated_at on the transform node itself", {
  ctx   <- make_ctx()
  tform <- lin_register_transform(ctx, "t1")

  run_id <- lin_run_start(ctx, tform)
  lin_run_complete(ctx, run_id)

  row <- DBI::dbGetQuery(ctx$con,
    "SELECT last_updated_at FROM lineage_nodes WHERE node_id = ?",
    params = list(tform))
  expect_false(is.na(row$last_updated_at))
})

test_that("lin_run_error sets status = error with message", {
  ctx   <- make_ctx()
  tform <- lin_register_transform(ctx, "t1")

  run_id <- lin_run_start(ctx, tform)
  lin_run_error(ctx, run_id, "connection timed out")

  row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_run_log WHERE run_id = ?", params = list(run_id))
  expect_equal(row$status,        "error")
  expect_equal(row$error_message, "connection timed out")
  expect_false(is.na(row$completed_at))
})

test_that("lin_run_history returns rows in descending started_at order", {
  ctx   <- make_ctx()
  tform <- lin_register_transform(ctx, "t1")

  for (i in seq_len(3L)) {
    r <- lin_run_start(ctx, tform)
    lin_run_complete(ctx, r, rows_produced = as.integer(i * 100L))
  }

  hist <- lin_run_history(ctx, tform, n = 10L)
  expect_equal(nrow(hist), 3L)
  expect_true(all(hist$status == "success"))
})

test_that("lin_run_history respects the n parameter", {
  ctx   <- make_ctx()
  tform <- lin_register_transform(ctx, "t1")

  for (i in seq_len(5L)) {
    r <- lin_run_start(ctx, tform)
    lin_run_complete(ctx, r)
  }

  hist <- lin_run_history(ctx, tform, n = 2L)
  expect_equal(nrow(hist), 2L)
})

test_that("lin_run_start aborts for unknown node", {
  ctx <- make_ctx()
  expect_error(lin_run_start(ctx, "no_such_node"), class = "rlang_error")
})

test_that("lin_run_complete aborts for unknown run_id", {
  ctx <- make_ctx()
  expect_error(lin_run_complete(ctx, "no_such_run"), class = "rlang_error")
})

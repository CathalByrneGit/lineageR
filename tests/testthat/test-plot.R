test_that("lin_plot renders without error (any available backend)", {
  skip_if_not_installed("igraph")
  f <- build_fixture()

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)

  expect_no_error(lin_plot(f$ctx))
})

test_that("lin_plot with highlight_node_id does not error", {
  skip_if_not_installed("igraph")
  f <- build_fixture()

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)

  expect_no_error(lin_plot(f$ctx, highlight_node_id = f$s3_id))
})

test_that("lin_plot with show_stale = FALSE does not error", {
  skip_if_not_installed("igraph")
  f <- build_fixture()

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)

  expect_no_error(lin_plot(f$ctx, show_stale = FALSE))
})

test_that("lin_plot with visNetwork returns an htmlwidget", {
  skip_if_not_installed("visNetwork")
  f      <- build_fixture()
  result <- lin_plot(f$ctx)
  expect_s3_class(result, "htmlwidget")
})

test_that("lin_plot with ggraph returns a ggplot object", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("ggplot2")
  skip_if(requireNamespace("visNetwork", quietly = TRUE),
          "visNetwork takes priority over ggraph path")
  f      <- build_fixture()
  result <- lin_plot(f$ctx)
  expect_s3_class(result, "gg")
})

test_that("lin_plot on an empty context does not error", {
  skip_if_not_installed("igraph")
  ctx <- make_ctx()

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)

  expect_no_error(lin_plot(ctx))
})

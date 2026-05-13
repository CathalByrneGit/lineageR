# ── Test helpers ──────────────────────────────────────────────────────────────

# Build the canonical fixture with OpenLineage enabled, writing to a temp file.
ol_fixture <- function() {
  f        <- build_fixture()
  ndjson   <- tempfile(fileext = ".ndjson")
  f$ctx    <- lin_openlineage_config(f$ctx, ndjson_path = ndjson, enabled = TRUE)
  f$ndjson <- ndjson
  f
}

# Read all events from the NDJSON file; returns a list of parsed events.
read_events <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0L]
  lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
}

last_event <- function(path) {
  evts <- read_events(path)
  if (length(evts) == 0L) return(NULL)
  evts[[length(evts)]]
}

# ── lin_openlineage_config ────────────────────────────────────────────────────

test_that("lin_openlineage_config returns a config list when ctx is omitted", {
  cfg <- lin_openlineage_config(url = "http://example.com/lineage",
                                 namespace = "test-ns")
  expect_type(cfg, "list")
  expect_equal(cfg$url,       "http://example.com/lineage")
  expect_equal(cfg$namespace, "test-ns")
  expect_true(cfg$enabled)
})

test_that("lin_openlineage_config attaches config to ctx and returns ctx", {
  ctx <- make_ctx()
  ctx2 <- lin_openlineage_config(ctx, namespace = "my-ns", enabled = TRUE,
                                  ndjson_path = tempfile())
  expect_s3_class(ctx2, "LineageContext")
  expect_equal(ctx2$ol_config$namespace, "my-ns")
  expect_true(ctx2$ol_config$enabled)
})

test_that("lin_openlineage_config rejects non-context first argument", {
  expect_error(
    lin_openlineage_config("http://not-a-context"),
    class = "rlang_error"
  )
})

test_that("OL_NAMESPACE env var overrides namespace argument", {
  withr::with_envvar(c(OL_NAMESPACE = "env-ns"), {
    cfg <- lin_openlineage_config()
    expect_equal(cfg$namespace, "env-ns")
  })
})

test_that("OL_ENABLED=false disables emission via env var", {
  withr::with_envvar(c(OL_ENABLED = "false"), {
    cfg <- lin_openlineage_config()
    expect_false(cfg$enabled)
  })
})

# ── lineage_context defaults ──────────────────────────────────────────────────

test_that("lineage_context initialises ol_config disabled by default", {
  ctx <- make_ctx()
  expect_false(is.null(ctx$ol_config))
  expect_false(ctx$ol_config$enabled)
})

# ── NDJSON emission: START ────────────────────────────────────────────────────

test_that("lin_run_start emits a START event to NDJSON", {
  f      <- ol_fixture()
  run_id <- lin_run_start(f$ctx, f$ingest_id)

  evts <- read_events(f$ndjson)
  expect_length(evts, 1L)

  ev <- evts[[1L]]
  expect_equal(ev$eventType,      "START")
  expect_equal(ev$job$name,       "ingest")
  expect_equal(ev$job$namespace,  f$ctx$ol_config$namespace)
  expect_equal(ev$run$runId,      run_id)
})

test_that("START event inputs list contains the s3_bucket node", {
  f <- ol_fixture()
  lin_run_start(f$ctx, f$ingest_id)

  ev      <- last_event(f$ndjson)
  in_names <- vapply(ev$inputs, `[[`, character(1), "name")
  expect_true("s3_bucket" %in% in_names)
})

test_that("START event outputs use include_schema = FALSE (no schema facet)", {
  f <- ol_fixture()
  lin_run_start(f$ctx, f$ingest_id)

  ev <- last_event(f$ndjson)
  for (out in ev$outputs) {
    expect_false("schema" %in% names(out$facets))
  }
})

# ── NDJSON emission: COMPLETE ─────────────────────────────────────────────────

test_that("lin_run_complete emits a COMPLETE event to NDJSON", {
  f      <- ol_fixture()
  run_id <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_complete(f$ctx, run_id, rows_produced = 100L)

  evts <- read_events(f$ndjson)
  expect_length(evts, 2L)   # START + COMPLETE

  ev <- evts[[2L]]
  expect_equal(ev$eventType, "COMPLETE")
  expect_equal(ev$job$name,  "ingest")
  expect_equal(ev$run$runId, run_id)
})

test_that("COMPLETE event includes inputs and outputs", {
  f      <- ol_fixture()
  run_id <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_complete(f$ctx, run_id)

  ev <- last_event(f$ndjson)
  expect_true(length(ev$inputs)  > 0L)
  expect_true(length(ev$outputs) > 0L)

  out_names <- vapply(ev$outputs, `[[`, character(1), "name")
  expect_true("raw" %in% out_names)
})

test_that("COMPLETE event eventTime is a non-empty ISO 8601 string", {
  f      <- ol_fixture()
  run_id <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_complete(f$ctx, run_id)

  ev <- last_event(f$ndjson)
  expect_match(ev$eventTime,
    "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

# ── NDJSON emission: FAIL ─────────────────────────────────────────────────────

test_that("lin_run_error emits a FAIL event to NDJSON", {
  f      <- ol_fixture()
  run_id <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_error(f$ctx, run_id, "Source table not found")

  ev <- last_event(f$ndjson)
  expect_equal(ev$eventType, "FAIL")
  expect_equal(ev$job$name,  "ingest")
})

test_that("FAIL event contains errorMessage facet in run facets", {
  f      <- ol_fixture()
  run_id <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_error(f$ctx, run_id, "connection refused")

  ev <- last_event(f$ndjson)
  em <- ev$run$facets$errorMessage
  expect_false(is.null(em))
  expect_equal(em$message, "connection refused")
  expect_equal(em$programmingLanguage, "R")
})

# ── enabled = FALSE ───────────────────────────────────────────────────────────

test_that("no events are written when enabled = FALSE", {
  f      <- build_fixture()
  ndjson <- tempfile(fileext = ".ndjson")
  f$ctx  <- lin_openlineage_config(f$ctx, ndjson_path = ndjson, enabled = FALSE)

  run_id <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_complete(f$ctx, run_id)

  expect_false(file.exists(ndjson))
})

# ── HTTP failure fallback ─────────────────────────────────────────────────────

test_that("HTTP failure falls back to NDJSON without throwing", {
  f      <- build_fixture()
  ndjson <- tempfile(fileext = ".ndjson")
  # Point at a port nothing is listening on
  f$ctx  <- lin_openlineage_config(f$ctx,
    url         = "http://127.0.0.1:19999/api/v1/lineage",
    ndjson_path = ndjson,
    enabled     = TRUE
  )
  skip_if_not_installed("httr2")

  run_id <- lin_run_start(f$ctx, f$ingest_id)

  expect_no_error(lin_run_complete(f$ctx, run_id))
  # Fell back to NDJSON
  expect_true(file.exists(ndjson))
})

# ── Existing tests unaffected (OL disabled by default) ────────────────────────

test_that("run functions still work when ol_config is absent from ctx", {
  ctx   <- make_ctx()
  ctx$ol_config <- NULL   # simulate pre-OL context

  tform  <- lin_register_transform(ctx, "t1")
  run_id <- lin_run_start(ctx, tform)
  expect_no_error(lin_run_complete(ctx, run_id))
  expect_no_error(lin_run_error(ctx, run_id, "err"))
})

# ── lin_replay_events ─────────────────────────────────────────────────────────

test_that("lin_replay_events warns when OL is disabled", {
  ctx <- make_ctx()   # OL disabled by default
  expect_warning(lin_replay_events(ctx), regexp = "disabled")
})

test_that("lin_replay_events emits one event per completed/failed run", {
  f      <- ol_fixture()
  ndjson <- f$ndjson

  # Three runs: 2 success, 1 error
  for (i in seq_len(2L)) {
    r <- lin_run_start(f$ctx, f$ingest_id)
    lin_run_complete(f$ctx, r)
  }
  r3 <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_error(f$ctx, r3, "oops")

  # Clear NDJSON and replay
  file.remove(ndjson)
  n <- lin_replay_events(f$ctx)

  expect_equal(n, 3L)
  evts <- read_events(ndjson)
  expect_length(evts, 3L)

  types <- vapply(evts, `[[`, character(1), "eventType")
  expect_equal(sum(types == "COMPLETE"), 2L)
  expect_equal(sum(types == "FAIL"),     1L)
})

test_that("lin_replay_events respects the since parameter", {
  f      <- ol_fixture()
  ndjson <- f$ndjson

  # Two runs with an artificial gap
  r1 <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_complete(f$ctx, r1)

  cutoff <- Sys.time()
  Sys.sleep(0.05)

  r2 <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_complete(f$ctx, r2)

  file.remove(ndjson)
  lin_replay_events(f$ctx, since = cutoff)

  evts <- read_events(ndjson)
  expect_length(evts, 1L)
})

# ── Event structure validation ────────────────────────────────────────────────

test_that("emitted event has all required OpenLineage envelope fields", {
  f      <- ol_fixture()
  run_id <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_complete(f$ctx, run_id)

  ev <- last_event(f$ndjson)
  expect_true(all(c("eventType", "eventTime", "run", "job",
                    "inputs", "outputs") %in% names(ev)))
  expect_true("runId"     %in% names(ev$run))
  expect_true("namespace" %in% names(ev$job))
  expect_true("name"      %in% names(ev$job))
})

test_that("schema validation against OpenLineage spec (requires jsonvalidate + network)", {
  skip_if_not_installed("jsonvalidate")
  skip_if_offline()

  f      <- ol_fixture()
  run_id <- lin_run_start(f$ctx, f$ingest_id)
  lin_run_complete(f$ctx, run_id)

  ev   <- last_event(f$ndjson)
  json <- jsonlite::toJSON(ev, auto_unbox = TRUE, null = "null")

  result <- jsonvalidate::json_validate(
    json,
    schema  = "https://openlineage.io/spec/2-0-2/OpenLineage.json",
    engine  = "ajv",
    verbose = TRUE
  )
  expect_true(result)
})

# ── lin_marquez_setup ─────────────────────────────────────────────────────────

test_that("lin_marquez_setup prints without error", {
  expect_no_error(lin_marquez_setup())
  expect_no_error(lin_marquez_setup(port = 5001L))
})

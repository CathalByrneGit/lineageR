# ── Configuration ─────────────────────────────────────────────────────────────

#' Configure OpenLineage event emission
#'
#' Call once per context to enable emission of
#' [OpenLineage](https://openlineage.io) events alongside lineageR's internal
#' tables. Events can be POSTed to a Marquez (or any OpenLineage-compatible)
#' HTTP backend, written to a local NDJSON file, or both.
#'
#' Environment variables are read at call time and override explicit arguments:
#' - `OL_URL` — HTTP backend URL
#' - `OL_NAMESPACE` — namespace
#' - `OL_ENABLED` — `"true"` / `"false"`
#'
#' @param ctx A `LineageContext` (optional).  When supplied, `ctx$ol_config` is
#'   updated and the modified context is returned invisibly.  When omitted, a
#'   plain config list is returned (useful for testing or for assigning to
#'   `ctx$ol_config` manually).
#' @param url Character or `NULL`.  OpenLineage HTTP backend URL, e.g.
#'   `"http://localhost:5000/api/v1/lineage"` for local Marquez.  `NULL` skips
#'   HTTP emission.
#' @param namespace Character.  OpenLineage namespace for all jobs and datasets
#'   emitted from this context.  Default `"default"`.
#' @param producer_url Character.  URL placed in every `_producer` field.
#' @param enabled Logical.  `FALSE` silences all emission.  Default `TRUE`.
#' @param ndjson_path Character or `NULL`.  Path to append NDJSON events to.
#'   `NULL` disables file output.  Default `"lineage_events.ndjson"`.
#' @return If `ctx` is a `LineageContext`, returns `ctx` invisibly with
#'   `ctx$ol_config` updated.  Otherwise returns the config list.
#' @export
lin_openlineage_config <- function(ctx         = NULL,
                                   url         = NULL,
                                   namespace   = "default",
                                   producer_url = "https://github.com/CathalByrneGit/lineageR",
                                   enabled     = TRUE,
                                   ndjson_path = "lineage_events.ndjson") {
  if (!is.null(ctx) && !inherits(ctx, "LineageContext")) {
    cli::cli_abort(
      "{.arg ctx} must be a {.cls LineageContext} or {.code NULL}, not {.type {ctx}}."
    )
  }

  # Environment variables override explicit arguments
  env_url <- Sys.getenv("OL_URL", unset = "")
  if (nchar(env_url) > 0L && is.null(url)) url <- env_url

  env_ns <- Sys.getenv("OL_NAMESPACE", unset = "")
  if (nchar(env_ns) > 0L) namespace <- env_ns

  env_en <- Sys.getenv("OL_ENABLED", unset = "")
  if (nchar(env_en) > 0L) enabled <- tolower(env_en) == "true"

  config <- list(
    url          = url,
    namespace    = namespace,
    producer_url = producer_url,
    enabled      = isTRUE(enabled),
    ndjson_path  = ndjson_path
  )

  if (inherits(ctx, "LineageContext")) {
    ctx$ol_config <- config
    return(invisible(ctx))
  }
  config
}

# Default config installed by lineage_context() — disabled unless env vars say otherwise
.default_ol_config <- function() {
  enabled <- tolower(Sys.getenv("OL_ENABLED", unset = "false")) == "true"
  url     <- Sys.getenv("OL_URL",       unset = "")
  ns      <- Sys.getenv("OL_NAMESPACE", unset = "default")
  list(
    url          = if (nchar(url) > 0L) url else NULL,
    namespace    = if (nchar(ns)  > 0L) ns  else "default",
    producer_url = "https://github.com/CathalByrneGit/lineageR",
    enabled      = enabled,
    ndjson_path  = "lineage_events.ndjson"
  )
}

# ── Replay / setup helpers ────────────────────────────────────────────────────

#' Replay existing run history as OpenLineage events
#'
#' Reads completed and failed runs from `lineage_run_log` and re-emits them via
#' the configured OpenLineage backend.  Useful for backfilling a Marquez
#' instance with historical data.
#'
#' @param ctx A `LineageContext` with `ol_config` set (see
#'   [lin_openlineage_config()]).
#' @param since `POSIXct` or `NULL`.  Only replay runs started after this
#'   timestamp.
#' @return Invisibly, the number of events emitted.
#' @export
lin_replay_events <- function(ctx, since = NULL) {
  if (is.null(ctx$ol_config) || !isTRUE(ctx$ol_config$enabled)) {
    cli::cli_warn(c(
      "!" = "OpenLineage emission is disabled.",
      "i" = "Enable it first with {.fn lin_openlineage_config}."
    ))
    return(invisible(0L))
  }

  sql    <- "SELECT * FROM lineage_run_log WHERE status IN ('success', 'error')"
  params <- list()
  if (!is.null(since)) {
    sql    <- paste(sql, "AND started_at > ?")
    params <- list(since)
  }
  runs <- DBI::dbGetQuery(ctx$con, sql, params = params)
  if (nrow(runs) == 0L) return(invisible(0L))

  emitted <- 0L
  for (i in seq_len(nrow(runs))) {
    run       <- as.list(runs[i, ])
    transform <- .ol_node(ctx, run$node_id)
    if (is.null(transform)) next

    inputs_df  <- .ol_inputs(ctx,  run$node_id)
    outputs_df <- .ol_outputs(ctx, run$node_id)

    event_type <- if (run$status == "success") "COMPLETE" else "FAIL"

    inputs  <- lapply(inputs_df$node_id,  function(nid)
      .ol_dataset(ctx, nid, include_schema = TRUE))
    outputs <- lapply(outputs_df$node_id, function(nid)
      .ol_dataset(ctx, nid, include_schema = TRUE,
                  column_lineage = .ol_column_lineage_facet(ctx, nid)))

    event <- .ol_run_event(
      ctx,
      run_id        = run$run_id,
      job_name      = transform$name,
      event_type    = event_type,
      inputs        = inputs,
      outputs       = outputs,
      error_message = if (event_type == "FAIL") run$error_message else NULL
    )
    .ol_emit(ctx, event)
    emitted <- emitted + 1L
  }

  invisible(emitted)
}

#' Print a Docker command to start Marquez locally
#'
#' @param port Integer. Port for the Marquez API. Default `5000L`.
#' @export
lin_marquez_setup <- function(port = 5000L) {
  ui_port  <- port + 1000L   # Marquez UI runs on API port + 1000
  cli::cli_inform(c(
    "i" = "Start Marquez with Docker:",
    " " = "",
    " " = "  docker run --name marquez \\",
    " " = "    -p {port}:{port} -p {ui_port}:{ui_port} \\",
    " " = "    marquezproject/marquez",
    " " = "",
    "i" = "Then configure lineageR:",
    " " = "",
    " " = "  ctx <- lin_openlineage_config(ctx,",
    " " = "    url       = \"http://localhost:{port}/api/v1/lineage\",",
    " " = "    namespace = \"your-ontology\"",
    " " = "  )",
    " " = "",
    "i" = "View lineage at {.url http://localhost:{ui_port}}"
  ))
  invisible(NULL)
}

# ── Internal: node / run lookup ───────────────────────────────────────────────

.ol_node <- function(ctx, node_id) {
  row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_nodes WHERE node_id = ?",
    params = list(node_id))
  if (nrow(row) == 0L) return(NULL)
  as.list(row[1L, ])
}

.ol_inputs <- function(ctx, transform_node_id) {
  DBI::dbGetQuery(ctx$con,
    "SELECT n.*
     FROM lineage_nodes n
     JOIN lineage_edges e ON n.node_id = e.from_node_id
     WHERE e.to_node_id = ?",
    params = list(transform_node_id))
}

.ol_outputs <- function(ctx, transform_node_id) {
  DBI::dbGetQuery(ctx$con,
    "SELECT n.*
     FROM lineage_nodes n
     JOIN lineage_edges e ON n.node_id = e.to_node_id
     WHERE e.from_node_id = ?",
    params = list(transform_node_id))
}

.ol_get_run <- function(ctx, run_id) {
  row <- DBI::dbGetQuery(ctx$con,
    "SELECT * FROM lineage_run_log WHERE run_id = ?",
    params = list(run_id))
  if (nrow(row) == 0L) return(NULL)
  as.list(row[1L, ])
}

# ── Internal: event builders ──────────────────────────────────────────────────

.ol_run_event <- function(ctx,
                           run_id,
                           job_name,
                           event_type,
                           inputs        = list(),
                           outputs       = list(),
                           error_message = NULL) {
  run_facets <- list()
  if (!is.null(error_message)) {
    run_facets$errorMessage <- list(
      `_producer`  = ctx$ol_config$producer_url,
      `_schemaURL` = "https://openlineage.io/spec/facets/1-0-0/ErrorMessageRunFacet.json",
      message      = as.character(error_message),
      programmingLanguage = "R"
    )
  }

  list(
    eventType = event_type,
    eventTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    run = list(
      runId  = run_id,
      facets = run_facets
    ),
    job = list(
      namespace = ctx$ol_config$namespace,
      name      = job_name,
      facets    = list()
    ),
    inputs  = inputs,
    outputs = outputs
  )
}

.ol_dataset <- function(ctx, node_id,
                         include_schema = TRUE,
                         column_lineage = NULL) {
  node <- .ol_node(ctx, node_id)
  if (is.null(node)) return(NULL)

  facets <- list()
  if (include_schema) {
    sf <- .ol_schema_facet(ctx, node_id)
    if (!is.null(sf)) facets$schema <- sf
  }
  if (!is.null(column_lineage)) {
    facets$columnLineage <- column_lineage
  }

  list(
    namespace = ctx$ol_config$namespace,
    name      = node$name,
    facets    = facets
  )
}

.ol_schema_facet <- function(ctx, node_id) {
  node <- .ol_node(ctx, node_id)
  if (is.null(node)) return(NULL)

  meta <- tryCatch(
    jsonlite::fromJSON(node$metadata_json %||% "{}", simplifyVector = FALSE),
    error = function(e) list()
  )
  table_name <- meta$table_name
  if (is.null(table_name)) return(NULL)

  fields <- tryCatch({
    cols <- DBI::dbGetQuery(ctx$con,
      "SELECT column_name, data_type
       FROM information_schema.columns
       WHERE table_name = ?",
      params = list(table_name))
    if (nrow(cols) == 0L) return(NULL)
    lapply(seq_len(nrow(cols)), function(i)
      list(name = cols$column_name[i], type = toupper(cols$data_type[i])))
  }, error = function(e) NULL)

  if (is.null(fields)) return(NULL)

  list(
    `_producer`  = ctx$ol_config$producer_url,
    `_schemaURL` = "https://openlineage.io/spec/facets/1-0-0/SchemaDatasetFacet.json",
    fields       = fields
  )
}

.ol_column_lineage_facet <- function(ctx, output_node_id) {
  # Checks for the optional cell_expression_lineage table; returns NULL if absent
  has_table <- tryCatch({
    DBI::dbGetQuery(ctx$con,
      "SELECT COUNT(*) AS n
       FROM information_schema.tables
       WHERE table_name = 'cell_expression_lineage'")$n > 0L
  }, error = function(e) FALSE)

  if (!has_table) return(NULL)

  expr_rows <- tryCatch(
    DBI::dbGetQuery(ctx$con,
      "SELECT * FROM cell_expression_lineage WHERE output_node_id = ?",
      params = list(output_node_id)),
    error = function(e) data.frame()
  )
  if (nrow(expr_rows) == 0L) return(NULL)

  fields <- list()
  for (i in seq_len(nrow(expr_rows))) {
    r  <- expr_rows[i, ]
    tf <- list(
      type        = "DIRECT",
      description = as.character(r$expression_text %||% "")
    )
    inp <- list(
      namespace       = ctx$ol_config$namespace,
      name            = as.character(r$input_dataset_name),
      field           = as.character(r$input_column),
      transformations = list(tf)
    )
    col <- as.character(r$output_column)
    if (is.null(fields[[col]])) {
      fields[[col]] <- list(inputFields = list())
    }
    fields[[col]]$inputFields <- c(fields[[col]]$inputFields, list(inp))
  }

  list(
    `_producer`  = ctx$ol_config$producer_url,
    `_schemaURL` = "https://openlineage.io/spec/facets/1-1-0/ColumnLineageDatasetFacet.json",
    fields       = fields
  )
}

# ── Internal: emission ────────────────────────────────────────────────────────

.ol_emit <- function(ctx, event) {
  cfg <- ctx$ol_config
  if (is.null(cfg) || !isTRUE(cfg$enabled)) return(invisible(NULL))

  # Filter NULL datasets from inputs/outputs
  event$inputs  <- Filter(Negate(is.null), event$inputs)
  event$outputs <- Filter(Negate(is.null), event$outputs)

  json <- tryCatch(
    jsonlite::toJSON(event, auto_unbox = TRUE, null = "null"),
    error = function(e) {
      cli::cli_warn("OpenLineage serialisation failed: {conditionMessage(e)}")
      return(invisible(NULL))
    }
  )
  if (is.null(json)) return(invisible(NULL))

  if (!is.null(cfg$url)) {
    http_ok <- tryCatch({
      if (!requireNamespace("httr2", quietly = TRUE)) {
        cli::cli_warn(
          "Install {.pkg httr2} to enable OpenLineage HTTP emission."
        )
        FALSE
      } else {
        httr2::request(cfg$url) |>
          httr2::req_headers("Content-Type" = "application/json") |>
          httr2::req_body_raw(json, type = "application/json") |>
          httr2::req_timeout(5L) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()
        TRUE
      }
    }, error = function(e) {
      cli::cli_warn(
        "OpenLineage HTTP emit to {.url {cfg$url}} failed: {conditionMessage(e)}"
      )
      FALSE
    })
    if (http_ok) return(invisible(NULL))
    # Fall through to NDJSON on HTTP failure
  }

  if (!is.null(cfg$ndjson_path)) {
    .ol_write_ndjson(cfg$ndjson_path, json)
  }

  invisible(NULL)
}

.ol_write_ndjson <- function(path, json) {
  tryCatch(
    cat(json, "\n", file = path, append = TRUE, sep = ""),
    error = function(e)
      cli::cli_warn("OpenLineage NDJSON write failed: {conditionMessage(e)}")
  )
}

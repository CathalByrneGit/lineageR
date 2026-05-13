#' Record the start of a transform run
#'
#' Inserts a row into `lineage_run_log` with `status = "running"` and emits
#' an OpenLineage `START` event if emission is configured.
#'
#' @param ctx A `LineageContext`.
#' @param transform_node_id `node_id` of the transform being executed.
#' @return The generated `run_id` (character).
#' @export
lin_run_start <- function(ctx, transform_node_id) {
  .check_node_exists(ctx, transform_node_id)
  run_id <- new_uuid()
  DBI::dbExecute(ctx$con,
    "INSERT INTO lineage_run_log
       (run_id, node_id, started_at, status)
     VALUES (?, ?, CURRENT_TIMESTAMP, 'running')",
    params = list(run_id, transform_node_id)
  )

  tryCatch({
    transform  <- .ol_node(ctx, transform_node_id)
    inputs_df  <- .ol_inputs(ctx,  transform_node_id)
    outputs_df <- .ol_outputs(ctx, transform_node_id)
    event <- .ol_run_event(
      ctx,
      run_id     = run_id,
      job_name   = transform$name,
      event_type = "START",
      inputs     = lapply(inputs_df$node_id, function(nid)
        .ol_dataset(ctx, nid, include_schema = TRUE)),
      outputs    = lapply(outputs_df$node_id, function(nid)
        .ol_dataset(ctx, nid, include_schema = FALSE))
    )
    .ol_emit(ctx, event)
  }, error = function(e) {
    cli::cli_warn("OpenLineage START event failed: {conditionMessage(e)}")
  })

  run_id
}

#' Record the successful completion of a transform run
#'
#' Sets `status = "success"`, updates `last_updated_at` on the transform node
#' and all its output dataset nodes, and emits an OpenLineage `COMPLETE` event
#' (including column-lineage facets when available).
#'
#' @param ctx A `LineageContext`.
#' @param run_id `run_id` returned by [lin_run_start()].
#' @param rows_produced Optional count of rows written.
#' @return Invisibly, `run_id`.
#' @export
lin_run_complete <- function(ctx, run_id, rows_produced = NULL) {
  run <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id FROM lineage_run_log WHERE run_id = ?",
    params = list(run_id)
  )
  if (nrow(run) == 0L) {
    cli::cli_abort("Run {.val {run_id}} not found in run log.")
  }

  now <- Sys.time()

  DBI::dbExecute(ctx$con,
    "UPDATE lineage_run_log
     SET status = 'success', completed_at = ?, rows_produced = ?
     WHERE run_id = ?",
    params = list(now, rows_produced, run_id)
  )

  transform_node_id <- run$node_id[1L]

  DBI::dbExecute(ctx$con,
    "UPDATE lineage_nodes SET last_updated_at = ? WHERE node_id = ?",
    params = list(now, transform_node_id)
  )

  output_nodes <- DBI::dbGetQuery(ctx$con,
    "SELECT to_node_id FROM lineage_edges WHERE from_node_id = ?",
    params = list(transform_node_id)
  )
  for (out_id in output_nodes$to_node_id) {
    DBI::dbExecute(ctx$con,
      "UPDATE lineage_nodes SET last_updated_at = ? WHERE node_id = ?",
      params = list(now, out_id)
    )
  }

  tryCatch({
    transform  <- .ol_node(ctx, transform_node_id)
    inputs_df  <- .ol_inputs(ctx,  transform_node_id)
    outputs_df <- .ol_outputs(ctx, transform_node_id)
    event <- .ol_run_event(
      ctx,
      run_id     = run_id,
      job_name   = transform$name,
      event_type = "COMPLETE",
      inputs     = lapply(inputs_df$node_id, function(nid)
        .ol_dataset(ctx, nid, include_schema = TRUE)),
      outputs    = lapply(outputs_df$node_id, function(nid)
        .ol_dataset(ctx, nid, include_schema = TRUE,
                    column_lineage = .ol_column_lineage_facet(ctx, nid)))
    )
    .ol_emit(ctx, event)
  }, error = function(e) {
    cli::cli_warn("OpenLineage COMPLETE event failed: {conditionMessage(e)}")
  })

  invisible(run_id)
}

#' Record a failed transform run
#'
#' Sets `status = "error"`, stores the error message, and emits an OpenLineage
#' `FAIL` event.
#'
#' @param ctx A `LineageContext`.
#' @param run_id `run_id` returned by [lin_run_start()].
#' @param error_message Character string describing the failure.
#' @return Invisibly, `run_id`.
#' @export
lin_run_error <- function(ctx, run_id, error_message) {
  now     <- Sys.time()
  run_row <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id FROM lineage_run_log WHERE run_id = ?",
    params = list(run_id)
  )
  DBI::dbExecute(ctx$con,
    "UPDATE lineage_run_log
     SET status = 'error', completed_at = ?, error_message = ?
     WHERE run_id = ?",
    params = list(now, as.character(error_message), run_id)
  )

  tryCatch({
    if (nrow(run_row) > 0L) {
      transform <- .ol_node(ctx, run_row$node_id[1L])
      if (!is.null(transform)) {
        event <- .ol_run_event(
          ctx,
          run_id        = run_id,
          job_name      = transform$name,
          event_type    = "FAIL",
          error_message = error_message
        )
        .ol_emit(ctx, event)
      }
    }
  }, error = function(e) {
    cli::cli_warn("OpenLineage FAIL event failed: {conditionMessage(e)}")
  })

  invisible(run_id)
}

#' Get recent run history for a transform
#'
#' @param ctx A `LineageContext`.
#' @param transform_node_id `node_id` of the transform.
#' @param n Maximum number of runs to return (most recent first).
#' @return Data frame with columns `run_id`, `started_at`, `completed_at`,
#'   `status`, `rows_produced`.
#' @export
lin_run_history <- function(ctx, transform_node_id, n = 20L) {
  DBI::dbGetQuery(ctx$con,
    "SELECT run_id, started_at, completed_at, status, rows_produced
     FROM lineage_run_log
     WHERE node_id = ?
     ORDER BY started_at DESC
     LIMIT ?",
    params = list(transform_node_id, as.integer(n))
  )
}

#' Create a lineage context
#'
#' Initialises the lineage schema tables in the given DBI connection and
#' returns a `LineageContext` object used by all other `lin_*` functions.
#'
#' @param connection A DBI connection. Can be the same connection used by
#'   objectSetsR / ontologySpecR.
#' @param bundle An ontologySpecR bundle (optional). When provided,
#'   [lin_sync_bundle()] is called to register all object type nodes.
#' @return A `LineageContext` S3 object.
#' @export
lineage_context <- function(connection, bundle = NULL) {
  ctx <- structure(
    list(con = connection, ol_config = .default_ol_config()),
    class = "LineageContext"
  )
  .init_schema(ctx)
  if (!is.null(bundle)) {
    lin_sync_bundle(ctx, bundle)
  }
  ctx
}

#' @export
print.LineageContext <- function(x, ...) {
  n_nodes <- DBI::dbGetQuery(x$con,
    "SELECT COUNT(*) AS n FROM lineage_nodes")$n
  n_edges <- DBI::dbGetQuery(x$con,
    "SELECT COUNT(*) AS n FROM lineage_edges")$n
  cli::cli_inform(c(
    "v" = "LineageContext",
    " " = "{n_nodes} node{?s}, {n_edges} edge{?s}"
  ))
  invisible(x)
}

.init_schema <- function(ctx) {
  con <- ctx$con
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS lineage_nodes (
      node_id         TEXT      NOT NULL,
      node_type       TEXT      NOT NULL,
      name            TEXT      NOT NULL,
      description     TEXT,
      metadata_json   TEXT,
      last_updated_at TIMESTAMP,
      created_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (node_id)
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS lineage_edges (
      edge_id      TEXT      NOT NULL,
      from_node_id TEXT      NOT NULL,
      to_node_id   TEXT      NOT NULL,
      edge_type    TEXT,
      created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (edge_id),
      FOREIGN KEY (from_node_id) REFERENCES lineage_nodes(node_id),
      FOREIGN KEY (to_node_id)   REFERENCES lineage_nodes(node_id)
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS lineage_run_log (
      run_id        TEXT      NOT NULL,
      node_id       TEXT      NOT NULL,
      started_at    TIMESTAMP NOT NULL,
      completed_at  TIMESTAMP,
      status        TEXT      NOT NULL,
      rows_produced INTEGER,
      error_message TEXT,
      PRIMARY KEY (run_id)
    )
  ")

  # ── Cell-level lineage tables ────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS cell_expression_lineage (
      expr_lineage_id   TEXT      NOT NULL,
      output_node_id    TEXT      NOT NULL,
      output_column     TEXT      NOT NULL,
      source_node_id    TEXT      NOT NULL,
      source_columns    TEXT      NOT NULL,
      transform_node_id TEXT      NOT NULL,
      expression_text   TEXT      NOT NULL,
      expression_hash   TEXT      NOT NULL,
      captured_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (expr_lineage_id),
      UNIQUE (output_node_id, output_column, transform_node_id)
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS row_provenance (
      provenance_id   TEXT      NOT NULL,
      output_node_id  TEXT      NOT NULL,
      output_pk_col   TEXT      NOT NULL,
      output_pk_value TEXT      NOT NULL,
      source_node_id  TEXT      NOT NULL,
      source_pk_col   TEXT      NOT NULL,
      source_pk_value TEXT      NOT NULL,
      join_type       TEXT,
      captured_at     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (provenance_id)
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS concept_cell_explanations (
      explanation_id  TEXT      NOT NULL,
      concept_id      TEXT      NOT NULL,
      scope           TEXT      NOT NULL,
      version         INTEGER   NOT NULL,
      object_pk_value TEXT      NOT NULL,
      overall_result  BOOLEAN   NOT NULL,
      sub_expressions TEXT      NOT NULL,
      traced_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (explanation_id)
    )
  ")
  tryCatch(
    DBI::dbExecute(con,
      "CREATE INDEX IF NOT EXISTS idx_row_prov_output
       ON row_provenance (output_node_id, output_pk_value)"),
    error = function(e) invisible(NULL)
  )

  invisible(ctx)
}

# ── Expression-level lineage ──────────────────────────────────────────────────

#' Record that an output column was derived from source columns via an expression
#'
#' Upserts a row into `cell_expression_lineage`.  If the same
#' `(output_node_id, output_column, transform_node_id)` triple already exists
#' it is replaced so the expression text / hash stay current.
#'
#' @param ctx A `LineageContext`.
#' @param transform_node_id Node ID of the transform step.
#' @param output_node_id Node ID of the output dataset.
#' @param output_column Name of the output column being derived.
#' @param source_node_ids Character vector (or scalar) of node IDs that provide
#'   input columns.
#' @param source_columns Character vector of source column names used.
#' @param expression_text The SQL or R expression string.
#' @return Invisibly, the `expr_lineage_id`.
#' @export
lin_record_expression <- function(ctx,
                                   transform_node_id,
                                   output_node_id,
                                   output_column,
                                   source_node_ids,
                                   source_columns,
                                   expression_text) {
  expr_hash     <- .hash_expression(expression_text)
  src_ids_json  <- jsonlite::toJSON(as.character(source_node_ids),
                                    auto_unbox = FALSE)
  src_cols_json <- jsonlite::toJSON(as.character(source_columns),
                                    auto_unbox = FALSE)

  DBI::dbExecute(ctx$con,
    "DELETE FROM cell_expression_lineage
     WHERE output_node_id = ? AND output_column = ? AND transform_node_id = ?",
    params = list(output_node_id, output_column, transform_node_id)
  )

  eid <- new_uuid()
  DBI::dbExecute(ctx$con,
    "INSERT INTO cell_expression_lineage
       (expr_lineage_id, output_node_id, output_column,
        source_node_id, source_columns, transform_node_id,
        expression_text, expression_hash)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(eid, output_node_id, output_column,
                  as.character(src_ids_json),
                  as.character(src_cols_json),
                  transform_node_id, expression_text, expr_hash)
  )
  invisible(eid)
}

#' Query column-level provenance for an output dataset
#'
#' @param ctx A `LineageContext`.
#' @param node_id Node ID of the output dataset to inspect.
#' @param column Optional character. Filter to a single output column.
#' @return Data frame with columns: `output_column`, `transform_name`,
#'   `expression_text`, `source_node_name`, `source_columns`, `captured_at`.
#' @export
lin_column_provenance <- function(ctx, node_id, column = NULL) {
  sql    <- "SELECT cel.output_column, cel.expression_text,
                    cel.source_node_id, cel.source_columns,
                    cel.captured_at,
                    tn.name AS transform_name
             FROM cell_expression_lineage cel
             JOIN lineage_nodes tn ON tn.node_id = cel.transform_node_id
             WHERE cel.output_node_id = ?"
  params <- list(node_id)

  if (!is.null(column)) {
    sql    <- paste(sql, "AND cel.output_column = ?")
    params <- c(params, list(column))
  }

  rows <- DBI::dbGetQuery(ctx$con, sql, params = params)

  empty <- data.frame(
    output_column    = character(0),
    transform_name   = character(0),
    expression_text  = character(0),
    source_node_name = character(0),
    source_columns   = character(0),
    captured_at      = character(0),
    stringsAsFactors = FALSE
  )
  if (nrow(rows) == 0L) return(empty)

  result_rows <- list()
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]

    src_ids <- tryCatch(
      jsonlite::fromJSON(as.character(r$source_node_id), simplifyVector = TRUE),
      error = function(e) character(0)
    )
    src_cols_str <- tryCatch({
      cols <- jsonlite::fromJSON(as.character(r$source_columns),
                                 simplifyVector = TRUE)
      paste(cols, collapse = ", ")
    }, error = function(e) "")

    if (length(src_ids) == 0L) {
      result_rows <- c(result_rows, list(data.frame(
        output_column    = r$output_column,
        transform_name   = r$transform_name,
        expression_text  = r$expression_text,
        source_node_name = NA_character_,
        source_columns   = src_cols_str,
        captured_at      = as.character(r$captured_at),
        stringsAsFactors = FALSE
      )))
    } else {
      for (nid in src_ids) {
        node     <- .ol_node(ctx, nid)
        src_name <- if (!is.null(node)) node$name else nid
        result_rows <- c(result_rows, list(data.frame(
          output_column    = r$output_column,
          transform_name   = r$transform_name,
          expression_text  = r$expression_text,
          source_node_name = src_name,
          source_columns   = src_cols_str,
          captured_at      = as.character(r$captured_at),
          stringsAsFactors = FALSE
        )))
      }
    }
  }
  do.call(rbind, result_rows)
}

#' Wrap a SQL statement and auto-capture column-level lineage
#'
#' Parses the SELECT list of `sql` for `alias AS expression` patterns, then
#' calls [lin_record_expression()] for each discovered output column.  The
#' original `sql` is returned unchanged — this function is a side-effect
#' helper.
#'
#' @param ctx A `LineageContext`.
#' @param transform_node_id Node ID of the transform.
#' @param sql The SQL SELECT string.
#' @param input_node_ids Character vector of source node IDs feeding this
#'   query.
#' @param output_node_id Node ID of the output dataset.
#' @return `sql`, invisibly.
#' @export
lin_wrap_sql <- function(ctx, transform_node_id, sql,
                          input_node_ids, output_node_id) {
  cols_info <- .parse_select_list(sql)
  for (col_info in cols_info) {
    src_cols <- parse_sql_column_refs(col_info$expression)
    tryCatch(
      lin_record_expression(
        ctx,
        transform_node_id = transform_node_id,
        output_node_id    = output_node_id,
        output_column     = col_info$alias,
        source_node_ids   = as.character(input_node_ids),
        source_columns    = src_cols,
        expression_text   = col_info$expression
      ),
      error = function(e) invisible(NULL)
    )
  }
  invisible(sql)
}

# ── Internal helpers ──────────────────────────────────────────────────────────

.hash_expression <- function(text) {
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(text, algo = "sha256", serialize = FALSE)
  } else {
    as.character(sum(utf8ToInt(as.character(text))))
  }
}

.parse_select_list <- function(sql) {
  sel_match <- regmatches(sql, regexpr(
    "(?i)SELECT\\s+(.+?)\\s+FROM\\b",
    sql, perl = TRUE
  ))
  if (length(sel_match) == 0L || nchar(sel_match) == 0L) return(list())

  sel_text <- regmatches(sel_match, regexpr(
    "(?i)(?<=SELECT\\s)(.+?)(?=\\s+FROM\\b)",
    sel_match, perl = TRUE
  ))
  if (length(sel_text) == 0L || nchar(sel_text) == 0L) return(list())

  parts <- .split_top_level_comma(sel_text)

  result <- list()
  for (part in parts) {
    part <- trimws(part)
    m <- regexec("(?i)^(.+?)\\s+AS\\s+([A-Za-z_][A-Za-z0-9_]*)$",
                 part, perl = TRUE)[[1L]]
    if (m[1L] != -1L) {
      raw <- regmatches(part, regexec(
        "(?i)^(.+?)\\s+AS\\s+([A-Za-z_][A-Za-z0-9_]*)$",
        part, perl = TRUE))[[1L]]
      result <- c(result, list(list(expression = trimws(raw[2L]),
                                    alias       = raw[3L])))
    } else {
      result <- c(result, list(list(expression = part, alias = part)))
    }
  }
  result
}

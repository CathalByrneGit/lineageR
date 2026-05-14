# ── Concept cell explanation ──────────────────────────────────────────────────

#' Explain why a concept evaluates to a value for one object
#'
#' Decomposes the concept's boolean expression into leaf sub-conditions,
#' evaluates each against the backing table for `object_pk_value`, traces
#' column- and row-level provenance, and stores the result in
#' `concept_cell_explanations`.
#'
#' @param ctx A `LineageContext`.
#' @param concept_ctx A concept context.  Must be a list with either a
#'   `get_concept(concept_id, version)` function or a `concepts` named list
#'   whose values are lists of version definition lists.  Each definition must
#'   have `expression` and, optionally, `table_name`, `pk_col`, `scope`, and
#'   `version`.
#' @param concept_id Character. Concept identifier.
#' @param scope Character. Scope / object-type name.
#' @param object_pk_value Character. Primary-key value of the object.
#' @param version Integer or `NULL`. Concept version; `NULL` uses the latest.
#' @return A `CellExplanation` S3 object.
#' @export
lin_explain_concept <- function(ctx, concept_ctx, concept_id, scope,
                                 object_pk_value, version = NULL) {
  concept_def <- .get_concept_def(concept_ctx, concept_id, version)
  ver         <- concept_def$version %||% 1L
  expr        <- concept_def$expression
  table_name  <- concept_def$table_name %||% scope
  pk_col      <- concept_def$pk_col     %||% "id"

  tree       <- decompose_sql_boolean(expr)
  conditions <- .collect_sub_conditions(tree)

  node <- .find_node_by_table(ctx, table_name)

  sub_results <- lapply(conditions, function(cond_expr) {
    val <- NA
    src <- NULL

    if (!is.null(node)) {
      val <- tryCatch({
        q <- sprintf(
          "SELECT CAST((%s) AS BOOLEAN) AS _result FROM %s WHERE %s = ?",
          cond_expr, table_name, pk_col
        )
        r <- DBI::dbGetQuery(ctx$con, q,
                             params = list(as.character(object_pk_value)))
        if (nrow(r) > 0L) r[["_result"]][1L] else NA
      }, error = function(e) NA)

      col_refs <- parse_sql_column_refs(cond_expr)
      for (col in col_refs) {
        cp <- tryCatch(
          lin_column_provenance(ctx, node$node_id, column = col),
          error = function(e) NULL
        )
        if (!is.null(cp) && nrow(cp) > 0L) {
          src <- cp
          break
        }
      }
    }

    list(expr = cond_expr, result = val, source_lineage = src)
  })

  overall <- tryCatch(
    all(vapply(sub_results, function(s) isTRUE(s$result), logical(1))),
    error = function(e) NA
  )

  eid      <- new_uuid()
  sub_json <- jsonlite::toJSON(
    lapply(sub_results, function(s) list(
      expr   = s$expr,
      result = if (is.na(s$result)) NULL else isTRUE(s$result)
    )),
    auto_unbox = TRUE, null = "null"
  )

  DBI::dbExecute(ctx$con,
    "INSERT INTO concept_cell_explanations
       (explanation_id, concept_id, scope, version, object_pk_value,
        overall_result, sub_expressions)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(eid, concept_id, scope, as.integer(ver),
                  as.character(object_pk_value),
                  isTRUE(overall), as.character(sub_json))
  )

  structure(
    list(
      explanation_id  = eid,
      concept_id      = concept_id,
      scope           = scope,
      version         = ver,
      object_pk_value = object_pk_value,
      overall_result  = overall,
      sub_expressions = sub_results
    ),
    class = "CellExplanation"
  )
}

#' Explain a concept for multiple objects
#'
#' Calls [lin_explain_concept()] for each element of `pk_values` and returns
#' a named list of `CellExplanation` objects.
#'
#' @param ctx A `LineageContext`.
#' @param concept_ctx Concept context (see [lin_explain_concept()]).
#' @param concept_id Character. Concept identifier.
#' @param scope Character. Scope / object-type name.
#' @param pk_values Character vector of primary-key values.
#' @param version Integer or `NULL`. Concept version.
#' @return Named list of `CellExplanation` objects (names = `pk_values`).
#' @export
lin_explain_concept_batch <- function(ctx, concept_ctx, concept_id, scope,
                                       pk_values, version = NULL) {
  results <- lapply(pk_values, function(pk) {
    tryCatch(
      lin_explain_concept(ctx, concept_ctx, concept_id, scope, pk, version),
      error = function(e) {
        cli::cli_warn("Explanation failed for {pk}: {conditionMessage(e)}")
        NULL
      }
    )
  })
  names(results) <- pk_values
  results
}

# ── Internal helpers ──────────────────────────────────────────────────────────

.get_concept_def <- function(concept_ctx, concept_id, version) {
  if (is.function(concept_ctx$get_concept)) {
    def <- concept_ctx$get_concept(concept_id, version)
  } else if (is.list(concept_ctx$concepts)) {
    defs <- concept_ctx$concepts[[concept_id]]
    if (is.null(version)) {
      def <- defs[[length(defs)]]
    } else {
      idx <- which(vapply(defs, function(d)
        identical(d$version, as.integer(version)), logical(1)))
      def <- if (length(idx) > 0L) defs[[idx[1L]]] else defs[[length(defs)]]
    }
  } else {
    cli::cli_abort(
      "Cannot extract concept {.val {concept_id}} from {.arg concept_ctx}."
    )
  }
  if (is.null(def)) {
    cli::cli_abort("Concept {.val {concept_id}} not found.")
  }
  def
}

.find_node_by_table <- function(ctx, table_name) {
  rows <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id, name, metadata_json FROM lineage_nodes"
  )
  for (i in seq_len(nrow(rows))) {
    meta <- tryCatch(
      jsonlite::fromJSON(rows$metadata_json[i] %||% "{}", simplifyVector = FALSE),
      error = function(e) list()
    )
    if (identical(meta$table_name, table_name)) {
      return(as.list(rows[i, ]))
    }
  }
  NULL
}

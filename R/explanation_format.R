# ── CellExplanation print / format ───────────────────────────────────────────

#' @export
print.CellExplanation <- function(x, ...) {
  overall_sym <- if (isTRUE(x$overall_result)) "✔" else "✘"

  cli::cli_inform(c(
    "v" = "CellExplanation",
    " " = "  Concept : {x$concept_id}",
    " " = "  Scope   : {x$scope}",
    " " = "  Version : {x$version}",
    " " = "  Object  : {x$object_pk_value}",
    " " = "  Result  : {overall_sym} {x$overall_result}"
  ))

  cli::cli_inform("  Sub-expressions:")
  for (s in x$sub_expressions) {
    sym <- if (isTRUE(s$result))   "✔" else
           if (isFALSE(s$result))  "✘" else "?"
    val_str <- if (is.null(s$result) || is.na(s$result)) "NA" else
               as.character(s$result)
    cli::cli_inform("    {sym} {s$expr}  [{val_str}]")
    if (!is.null(s$source_lineage) && nrow(s$source_lineage) > 0L) {
      sl <- s$source_lineage[1L, ]
      cli::cli_inform("        <- {sl$source_node_name} via {sl$transform_name}")
    }
  }
  invisible(x)
}

#' @export
format.CellExplanation <- function(x, ...) {
  overall_str <- if (isTRUE(x$overall_result)) "TRUE" else "FALSE"
  sub_strs <- vapply(x$sub_expressions, function(s) {
    res_str <- if (is.null(s$result) || is.na(s$result)) "NA" else
               as.character(s$result)
    sprintf("  [%s] %s", res_str, s$expr)
  }, character(1))
  paste(
    sprintf("CellExplanation: %s / %s / %s = %s",
            x$concept_id, x$scope, x$object_pk_value, overall_str),
    paste(sub_strs, collapse = "\n"),
    sep = "\n"
  )
}

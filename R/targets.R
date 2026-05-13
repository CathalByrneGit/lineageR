#' Generate lineage nodes and edges from a targets pipeline
#'
#' Reads a **targets** pipeline manifest and registers each target as a
#' `transform` node, linking dependencies with `"produces"` edges.
#'
#' @param ctx A `LineageContext`.
#' @param pipeline_path Path to the targets pipeline script (default:
#'   `"_targets.R"`).
#' @return Invisibly, a named list mapping target names to `node_id`s.
#' @export
lin_from_targets <- function(ctx, pipeline_path = "_targets.R") {
  if (!requireNamespace("targets", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg targets} package is required for {.fn lin_from_targets}."
    )
  }

  if (!file.exists(pipeline_path)) {
    cli::cli_abort("Pipeline file {.path {pipeline_path}} not found.")
  }

  manifest <- targets::tar_manifest(script          = pipeline_path,
                                    callr_function  = NULL)

  node_ids <- list()

  # First pass: create a transform node for each target
  for (i in seq_len(nrow(manifest))) {
    nid <- lin_register_transform(ctx,
      name     = manifest$name[i],
      language = "r"
    )
    node_ids[[manifest$name[i]]] <- nid
  }

  # Second pass: wire dependency edges (dep -> current)
  for (i in seq_len(nrow(manifest))) {
    deps <- manifest$deps[[i]]   # list column -> character vector
    if (is.null(deps) || length(deps) == 0L) next
    for (dep in deps) {
      if (dep %in% names(node_ids)) {
        .add_edge(ctx, node_ids[[dep]], node_ids[[manifest$name[i]]], "produces")
      }
    }
  }

  invisible(node_ids)
}

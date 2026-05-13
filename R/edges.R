#' Register that a transform consumes a source or dataset
#'
#' Adds a directed edge `input_node → transform_node` with type `"consumes"`.
#'
#' @param ctx A `LineageContext`.
#' @param transform_node_id `node_id` of the transform.
#' @param input_node_id `node_id` of the upstream source or dataset.
#' @return Invisibly, the `edge_id`.
#' @export
lin_add_input <- function(ctx, transform_node_id, input_node_id) {
  .add_edge(ctx, input_node_id, transform_node_id, "consumes")
}

#' Register that a transform produces a dataset
#'
#' Adds a directed edge `transform_node → output_node` with type `"produces"`.
#'
#' @param ctx A `LineageContext`.
#' @param transform_node_id `node_id` of the transform.
#' @param output_node_id `node_id` of the output dataset.
#' @return Invisibly, the `edge_id`.
#' @export
lin_add_output <- function(ctx, transform_node_id, output_node_id) {
  .add_edge(ctx, transform_node_id, output_node_id, "produces")
}

#' Register that a dataset backs an object type
#'
#' Adds a directed edge `dataset_node → object_type_node` with type `"backs"`.
#'
#' @param ctx A `LineageContext`.
#' @param dataset_node_id `node_id` of the backing dataset.
#' @param object_type_node_id `node_id` of the object type.
#' @return Invisibly, the `edge_id`.
#' @export
lin_add_backing <- function(ctx, dataset_node_id, object_type_node_id) {
  .add_edge(ctx, dataset_node_id, object_type_node_id, "backs")
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.check_node_exists <- function(ctx, node_id) {
  n <- DBI::dbGetQuery(ctx$con,
    "SELECT COUNT(*) AS n FROM lineage_nodes WHERE node_id = ?",
    params = list(node_id)
  )$n
  if (n == 0L) {
    cli::cli_abort("Node {.val {node_id}} does not exist in the lineage graph.")
  }
}

.would_create_cycle <- function(ctx, from_node_id, to_node_id) {
  # Would adding from -> to create a cycle?
  # Yes iff to can already reach from (i.e., from is a descendant of to).
  edges <- DBI::dbGetQuery(ctx$con,
    "SELECT from_node_id, to_node_id FROM lineage_edges"
  )
  if (nrow(edges) == 0L) return(FALSE)

  visited <- character(0)
  queue   <- to_node_id

  while (length(queue) > 0L) {
    current <- queue[1L]
    queue   <- queue[-1L]
    if (current %in% visited) next
    visited <- c(visited, current)
    if (current == from_node_id) return(TRUE)
    children <- edges$to_node_id[edges$from_node_id == current]
    queue <- c(queue, children[!children %in% visited])
  }

  FALSE
}

.add_edge <- function(ctx, from_node_id, to_node_id, edge_type) {
  .check_node_exists(ctx, from_node_id)
  .check_node_exists(ctx, to_node_id)

  if (.would_create_cycle(ctx, from_node_id, to_node_id)) {
    cli::cli_abort(c(
      "Adding edge {.val {from_node_id}} -> {.val {to_node_id}} would create a cycle.",
      "i" = "The lineage DAG must remain acyclic."
    ))
  }

  existing <- DBI::dbGetQuery(ctx$con,
    "SELECT edge_id FROM lineage_edges
     WHERE from_node_id = ? AND to_node_id = ?",
    params = list(from_node_id, to_node_id)
  )
  if (nrow(existing) > 0L) {
    return(invisible(existing$edge_id[1L]))
  }

  edge_id <- new_uuid()
  DBI::dbExecute(ctx$con,
    "INSERT INTO lineage_edges
       (edge_id, from_node_id, to_node_id, edge_type, created_at)
     VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)",
    params = list(edge_id, from_node_id, to_node_id, edge_type)
  )

  invisible(edge_id)
}

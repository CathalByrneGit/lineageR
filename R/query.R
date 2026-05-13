#' Get the full lineage DAG as an igraph object
#'
#' @param ctx A `LineageContext`.
#' @return A directed `igraph` graph. Vertex attributes: `node_id` (= vertex
#'   name), `node_type`, `label` (human name), `last_updated_at`, `stale`
#'   (logical). Edge attributes: `edge_id`, `edge_type`.
#' @export
lin_graph <- function(ctx) {
  nodes <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id, node_type, name, last_updated_at FROM lineage_nodes"
  )
  edges <- DBI::dbGetQuery(ctx$con,
    "SELECT edge_id, from_node_id, to_node_id, edge_type FROM lineage_edges"
  )

  vertex_df <- data.frame(
    name            = nodes$node_id,
    node_type       = nodes$node_type,
    label           = nodes$name,
    last_updated_at = nodes$last_updated_at,
    stringsAsFactors = FALSE
  )

  if (nrow(edges) == 0L) {
    edge_df <- data.frame(from = character(0), to = character(0),
                          stringsAsFactors = FALSE)
  } else {
    edge_df <- data.frame(from = edges$from_node_id,
                          to   = edges$to_node_id,
                          stringsAsFactors = FALSE)
  }

  g <- igraph::graph_from_data_frame(d = edge_df, directed = TRUE,
                                     vertices = vertex_df)

  stale_vec <- .compute_stale(nodes, edges)
  igraph::V(g)$stale <- stale_vec[igraph::V(g)$name]

  if (nrow(edges) > 0L) {
    igraph::E(g)$edge_id   <- edges$edge_id
    igraph::E(g)$edge_type <- edges$edge_type
  }

  g
}

#' Get upstream provenance for a node
#'
#' Returns a data frame of all ancestor nodes (upstream), optionally limited to
#' a maximum path `depth`.
#'
#' @param ctx A `LineageContext`.
#' @param node_id The node whose ancestors to find.
#' @param depth Maximum number of hops upstream. `NULL` (default) returns all
#'   ancestors.
#' @return Data frame with columns `node_id`, `node_type`, `name`, `distance`,
#'   `last_updated_at`.
#' @export
lin_provenance <- function(ctx, node_id, depth = NULL) {
  edges <- DBI::dbGetQuery(ctx$con,
    "SELECT from_node_id, to_node_id FROM lineage_edges"
  )
  nodes <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id, node_type, name, last_updated_at FROM lineage_nodes"
  )

  result  <- .bfs_upstream(node_id, edges, depth)
  if (nrow(result) == 0L) return(.empty_lineage_df())

  out <- merge(result, nodes, by = "node_id")
  out[, c("node_id", "node_type", "name", "distance", "last_updated_at")]
}

#' Get downstream impact for a node
#'
#' Returns a data frame of all descendant nodes (downstream).  Answers: *if
#' this node changes, what is affected?*
#'
#' @param ctx A `LineageContext`.
#' @param node_id The node whose descendants to find.
#' @param depth Maximum number of hops downstream. `NULL` returns all
#'   descendants.
#' @return Data frame with columns `node_id`, `node_type`, `name`, `distance`,
#'   `last_updated_at`.
#' @export
lin_impact <- function(ctx, node_id, depth = NULL) {
  edges <- DBI::dbGetQuery(ctx$con,
    "SELECT from_node_id, to_node_id FROM lineage_edges"
  )
  nodes <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id, node_type, name, last_updated_at FROM lineage_nodes"
  )

  result <- .bfs_downstream(node_id, edges, depth)
  if (nrow(result) == 0L) return(.empty_lineage_df())

  out <- merge(result, nodes, by = "node_id")
  out[, c("node_id", "node_type", "name", "distance", "last_updated_at")]
}

#' Get stale nodes
#'
#' A node is stale when any upstream ancestor has a `last_updated_at` timestamp
#' newer than the node's own `last_updated_at`.  Nodes without a
#' `last_updated_at` are excluded (freshness cannot be determined).
#'
#' @param ctx A `LineageContext`.
#' @return Data frame with columns `node_id`, `name`, `node_type`,
#'   `stale_since`, `stale_upstream_node`.
#' @export
lin_stale <- function(ctx) {
  nodes <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id, node_type, name, last_updated_at FROM lineage_nodes"
  )
  edges <- DBI::dbGetQuery(ctx$con,
    "SELECT from_node_id, to_node_id FROM lineage_edges"
  )

  empty <- data.frame(
    node_id            = character(0),
    name               = character(0),
    node_type          = character(0),
    stale_since        = as.POSIXct(character(0)),
    stale_upstream_node = character(0),
    stringsAsFactors   = FALSE
  )

  if (nrow(nodes) == 0L || nrow(edges) == 0L) return(empty)

  node_ts    <- setNames(nodes$last_updated_at, nodes$node_id)
  node_names <- setNames(nodes$name,            nodes$node_id)

  rows <- vector("list", nrow(nodes))

  for (i in seq_len(nrow(nodes))) {
    nid       <- nodes$node_id[i]
    node_time <- nodes$last_updated_at[i]

    if (is.na(node_time)) next

    ancestors <- .all_ancestors(nid, edges)
    if (length(ancestors) == 0L) next

    anc_times  <- node_ts[ancestors]
    valid      <- !is.na(anc_times)
    if (!any(valid)) next

    max_anc_time <- max(anc_times[valid])
    if (max_anc_time > node_time) {
      stale_upstream <- names(which.max(anc_times[valid]))
      rows[[i]] <- data.frame(
        node_id             = nid,
        name                = nodes$name[i],
        node_type           = nodes$node_type[i],
        stale_since         = node_time,
        stale_upstream_node = node_names[stale_upstream],
        stringsAsFactors    = FALSE
      )
    }
  }

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(empty)
  do.call(rbind, rows)
}

#' Get the lineage path between two nodes
#'
#' Returns the ordered data frame of nodes on the shortest directed path from
#' `from_node_id` to `to_node_id`, or `NULL` if no path exists.
#'
#' @param ctx A `LineageContext`.
#' @param from_node_id Starting node.
#' @param to_node_id Ending node.
#' @return Data frame (columns: `node_id`, `node_type`, `name`,
#'   `last_updated_at`, `order`), or `NULL`.
#' @export
lin_path <- function(ctx, from_node_id, to_node_id) {
  edges <- DBI::dbGetQuery(ctx$con,
    "SELECT from_node_id, to_node_id FROM lineage_edges"
  )
  nodes <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id, node_type, name, last_updated_at FROM lineage_nodes"
  )

  if (nrow(edges) == 0L) return(NULL)

  prev    <- list()
  visited <- character(0)
  queue   <- from_node_id
  found   <- FALSE

  while (length(queue) > 0L && !found) {
    current <- queue[1L]
    queue   <- queue[-1L]
    if (current %in% visited) next
    visited <- c(visited, current)
    if (current == to_node_id) { found <- TRUE; break }
    children <- edges$to_node_id[edges$from_node_id == current]
    for (child in children) {
      if (!child %in% visited && is.null(prev[[child]])) {
        prev[[child]] <- current
        queue <- c(queue, child)
      }
    }
  }

  if (!found) return(NULL)

  path    <- character(0)
  current <- to_node_id
  while (current != from_node_id) {
    path    <- c(current, path)
    current <- prev[[current]]
    if (is.null(current)) return(NULL)
  }
  path <- c(from_node_id, path)

  path_df        <- nodes[nodes$node_id %in% path, ]
  path_df        <- path_df[match(path, path_df$node_id), ]
  path_df$order  <- seq_len(nrow(path_df))
  rownames(path_df) <- NULL
  path_df
}

# ---------------------------------------------------------------------------
# Internal graph helpers
# ---------------------------------------------------------------------------

.bfs_upstream <- function(node_id, edges, depth) {
  result  <- data.frame(node_id = character(0), distance = integer(0),
                        stringsAsFactors = FALSE)
  visited <- character(0)
  queue   <- data.frame(id = node_id, dist = 0L, stringsAsFactors = FALSE)

  while (nrow(queue) > 0L) {
    cur_id   <- queue$id[1L]
    cur_dist <- queue$dist[1L]
    queue    <- queue[-1L, , drop = FALSE]

    if (cur_id %in% visited) next
    if (cur_id != node_id) {
      result <- rbind(result,
        data.frame(node_id = cur_id, distance = cur_dist,
                   stringsAsFactors = FALSE))
    }
    visited <- c(visited, cur_id)

    if (!is.null(depth) && cur_dist >= depth) next

    parents <- edges$from_node_id[edges$to_node_id == cur_id]
    new_p   <- parents[!parents %in% visited]
    if (length(new_p) > 0L) {
      queue <- rbind(queue,
        data.frame(id = new_p, dist = cur_dist + 1L,
                   stringsAsFactors = FALSE))
    }
  }

  result
}

.bfs_downstream <- function(node_id, edges, depth) {
  result  <- data.frame(node_id = character(0), distance = integer(0),
                        stringsAsFactors = FALSE)
  visited <- character(0)
  queue   <- data.frame(id = node_id, dist = 0L, stringsAsFactors = FALSE)

  while (nrow(queue) > 0L) {
    cur_id   <- queue$id[1L]
    cur_dist <- queue$dist[1L]
    queue    <- queue[-1L, , drop = FALSE]

    if (cur_id %in% visited) next
    if (cur_id != node_id) {
      result <- rbind(result,
        data.frame(node_id = cur_id, distance = cur_dist,
                   stringsAsFactors = FALSE))
    }
    visited <- c(visited, cur_id)

    if (!is.null(depth) && cur_dist >= depth) next

    children <- edges$to_node_id[edges$from_node_id == cur_id]
    new_c    <- children[!children %in% visited]
    if (length(new_c) > 0L) {
      queue <- rbind(queue,
        data.frame(id = new_c, dist = cur_dist + 1L,
                   stringsAsFactors = FALSE))
    }
  }

  result
}

.all_ancestors <- function(node_id, edges) {
  if (nrow(edges) == 0L) return(character(0))
  visited <- character(0)
  queue   <- node_id
  while (length(queue) > 0L) {
    current <- queue[1L]
    queue   <- queue[-1L]
    if (current %in% visited) next
    if (current != node_id) visited <- c(visited, current)
    parents <- edges$from_node_id[edges$to_node_id == current]
    queue   <- c(queue, parents[!parents %in% visited & parents != node_id])
  }
  visited
}

.compute_stale <- function(nodes, edges) {
  stale <- setNames(rep(FALSE, nrow(nodes)), nodes$node_id)
  if (nrow(nodes) == 0L || nrow(edges) == 0L) return(stale)

  node_ts <- setNames(nodes$last_updated_at, nodes$node_id)

  for (nid in nodes$node_id) {
    node_time <- node_ts[[nid]]
    if (is.na(node_time)) next

    ancestors <- .all_ancestors(nid, edges)
    if (length(ancestors) == 0L) next

    anc_times <- node_ts[ancestors]
    valid     <- !is.na(anc_times)
    if (!any(valid)) next

    if (max(anc_times[valid]) > node_time) stale[[nid]] <- TRUE
  }

  stale
}

.empty_lineage_df <- function() {
  data.frame(
    node_id         = character(0),
    node_type       = character(0),
    name            = character(0),
    distance        = integer(0),
    last_updated_at = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )
}

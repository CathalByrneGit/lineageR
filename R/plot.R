#' Plot the lineage DAG
#'
#' Uses **visNetwork** when available, falls back to **ggraph** + **ggplot2**,
#' and finally to base **igraph** plotting.
#'
#' Node colours by type: source = blue, transform = orange, dataset = green,
#' object_type = purple.  Stale nodes are shown with a red border when
#' `show_stale = TRUE`.
#'
#' @param ctx A `LineageContext`.
#' @param highlight_node_id Optional `node_id` to highlight (yellow fill).
#' @param show_stale Logical; draw a red border around stale nodes.
#' @param layout Layout algorithm passed to ggraph / igraph. One of
#'   `"sugiyama"`, `"fr"`, `"kk"`.
#' @return A visNetwork widget, a ggplot object, or `invisible(NULL)` for the
#'   base igraph path.
#' @export
lin_plot <- function(ctx,
                     highlight_node_id = NULL,
                     show_stale        = TRUE,
                     layout            = c("sugiyama", "fr", "kk")) {
  layout <- match.arg(layout)
  g      <- lin_graph(ctx)

  type_colors <- c(
    source      = "#4472C4",
    transform   = "#ED7D31",
    dataset     = "#70AD47",
    object_type = "#7030A0"
  )
  node_types  <- igraph::V(g)$node_type
  node_colors <- type_colors[node_types]
  node_colors[is.na(node_colors)] <- "#999999"

  stale_flags <- if (show_stale) igraph::V(g)$stale else rep(FALSE, igraph::vcount(g))
  stale_flags[is.na(stale_flags)] <- FALSE

  if (requireNamespace("visNetwork", quietly = TRUE)) {
    return(.lin_plot_visnetwork(g, node_colors, stale_flags, highlight_node_id))
  }

  if (requireNamespace("ggraph",   quietly = TRUE) &&
      requireNamespace("ggplot2",  quietly = TRUE)) {
    return(.lin_plot_ggraph(g, node_colors, stale_flags, highlight_node_id, layout))
  }

  # Base igraph fallback
  border_colors <- ifelse(stale_flags, "red", node_colors)
  if (!is.null(highlight_node_id)) {
    hit <- which(igraph::V(g)$name == highlight_node_id)
    if (length(hit) > 0L) node_colors[hit] <- "yellow"
  }
  igraph::plot.igraph(
    g,
    vertex.color       = node_colors,
    vertex.frame.color = border_colors,
    vertex.label       = igraph::V(g)$label,
    vertex.label.cex   = 0.8,
    edge.arrow.size    = 0.5,
    main               = "Lineage DAG"
  )
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Back-end helpers
# ---------------------------------------------------------------------------

.lin_plot_visnetwork <- function(g, node_colors, stale_flags,
                                 highlight_node_id) {
  node_ids <- igraph::V(g)$name
  labels   <- igraph::V(g)$label %||% node_ids
  types    <- igraph::V(g)$node_type

  bg <- node_colors
  if (!is.null(highlight_node_id)) {
    hit <- which(node_ids == highlight_node_id)
    if (length(hit) > 0L) bg[hit] <- "yellow"
  }

  nodes_df <- data.frame(
    id             = node_ids,
    label          = labels,
    group          = types,
    color.background = bg,
    color.border   = ifelse(stale_flags, "red", bg),
    borderWidth    = ifelse(stale_flags, 3L, 1L),
    title          = paste0("Type: ", types),
    stringsAsFactors = FALSE
  )

  el       <- igraph::as_edgelist(g)
  edges_df <- if (nrow(el) > 0L) {
    data.frame(from = el[, 1], to = el[, 2], arrows = "to",
               stringsAsFactors = FALSE)
  } else {
    data.frame(from = character(0), to = character(0),
               arrows = character(0), stringsAsFactors = FALSE)
  }

  visNetwork::visNetwork(nodes_df, edges_df, main = "Lineage DAG") |>
    visNetwork::visHierarchicalLayout(direction = "LR")
}

.lin_plot_ggraph <- function(g, node_colors, stale_flags,
                             highlight_node_id, layout) {
  labels <- igraph::V(g)$label %||% igraph::V(g)$name
  types  <- igraph::V(g)$node_type

  if (!is.null(highlight_node_id)) {
    hit <- which(igraph::V(g)$name == highlight_node_id)
    if (length(hit) > 0L) node_colors[hit] <- "yellow"
  }

  igraph::V(g)$color      <- node_colors
  igraph::V(g)$node_label <- labels
  igraph::V(g)$border_col <- ifelse(stale_flags, "red", "#333333")

  ggraph::ggraph(g, layout = layout) +
    ggraph::geom_edge_link(
      arrow    = grid::arrow(length = grid::unit(3, "mm"), type = "closed"),
      end_cap  = ggraph::circle(5, "mm"),
      color    = "#555555"
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(color = types),
      size = 8
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = labels),
      size  = 3
    ) +
    ggplot2::scale_color_manual(
      values = c(source      = "#4472C4",
                 transform   = "#ED7D31",
                 dataset     = "#70AD47",
                 object_type = "#7030A0"),
      name = "Node Type",
      na.value = "#999999"
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Lineage DAG")
}

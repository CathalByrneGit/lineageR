# ── Row-level provenance ──────────────────────────────────────────────────────

#' Record which source rows contributed to each output row
#'
#' Inserts provenance mappings into `row_provenance`.  When `mapping` exceeds
#' 100 000 rows a random 10 000-row sample is stored instead.
#'
#' @param ctx A `LineageContext`.
#' @param output_node_id Node ID of the output dataset.
#' @param output_pk_col Primary-key column name in the output dataset.
#' @param source_node_id Node ID of the source dataset.
#' @param source_pk_col Primary-key column name in the source dataset.
#' @param mapping A two-column data frame: `output_pk_value`,
#'   `source_pk_value`.
#' @param join_type Character. Label for the join, e.g. `"inner"`, `"left"`.
#'   Default `"direct"`.
#' @return Invisibly, the number of rows inserted.
#' @export
lin_record_row_provenance <- function(ctx,
                                       output_node_id,
                                       output_pk_col,
                                       source_node_id,
                                       source_pk_col,
                                       mapping,
                                       join_type = "direct") {
  if (nrow(mapping) > 100000L) {
    mapping <- mapping[sample.int(nrow(mapping), 10000L), ]
  }

  for (i in seq_len(nrow(mapping))) {
    pid <- new_uuid()
    DBI::dbExecute(ctx$con,
      "INSERT INTO row_provenance
         (provenance_id, output_node_id, output_pk_col, output_pk_value,
          source_node_id, source_pk_col, source_pk_value, join_type)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(pid, output_node_id, output_pk_col,
                    as.character(mapping$output_pk_value[i]),
                    source_node_id, source_pk_col,
                    as.character(mapping$source_pk_value[i]),
                    join_type)
    )
  }
  invisible(nrow(mapping))
}

#' Look up which source rows produced a given output row
#'
#' Performs a BFS over `row_provenance` to find the source rows at up to
#' `depth` hops upstream.
#'
#' @param ctx A `LineageContext`.
#' @param node_id Node ID of the output dataset.
#' @param pk_value Primary-key value of the output row (coerced to character).
#' @param depth Integer. How many provenance hops to traverse. Default `1L`.
#' @return Data frame with columns `source_node_id`, `source_node_name`,
#'   `source_pk_col`, `source_pk_value`, `join_type`, `depth`.
#' @export
lin_row_provenance <- function(ctx, node_id, pk_value, depth = 1L) {
  empty <- data.frame(
    source_node_id   = character(0),
    source_node_name = character(0),
    source_pk_col    = character(0),
    source_pk_value  = character(0),
    join_type        = character(0),
    depth            = integer(0),
    stringsAsFactors = FALSE
  )

  result  <- list()
  current <- data.frame(
    node_id  = as.character(node_id),
    pk_value = as.character(pk_value),
    stringsAsFactors = FALSE
  )

  for (d in seq_len(as.integer(depth))) {
    if (nrow(current) == 0L) break
    next_rows <- list()

    for (i in seq_len(nrow(current))) {
      rows <- DBI::dbGetQuery(ctx$con,
        "SELECT rp.source_node_id, rp.source_pk_col, rp.source_pk_value,
                rp.join_type, n.name AS source_node_name
         FROM row_provenance rp
         JOIN lineage_nodes n ON n.node_id = rp.source_node_id
         WHERE rp.output_node_id = ? AND rp.output_pk_value = ?",
        params = list(current$node_id[i], current$pk_value[i])
      )
      if (nrow(rows) > 0L) {
        rows$depth <- d
        result     <- c(result, list(rows))
        next_rows  <- c(next_rows, list(
          data.frame(node_id  = rows$source_node_id,
                     pk_value = rows$source_pk_value,
                     stringsAsFactors = FALSE)
        ))
      }
    }

    current <- if (length(next_rows) > 0L) {
      unique(do.call(rbind, next_rows))
    } else {
      data.frame(node_id  = character(0),
                 pk_value = character(0),
                 stringsAsFactors = FALSE)
    }
  }

  if (length(result) == 0L) return(empty)
  do.call(rbind, result)
}

#' Find all output rows derived from a source row
#'
#' Traverses `row_provenance` in the downstream direction.
#'
#' @param ctx A `LineageContext`.
#' @param source_node_id Node ID of the source dataset.
#' @param source_pk_value Primary-key value in the source dataset.
#' @return Data frame with columns `output_node_id`, `output_node_name`,
#'   `output_pk_col`, `output_pk_value`, `join_type`.
#' @export
lin_impact_rows <- function(ctx, source_node_id, source_pk_value) {
  empty <- data.frame(
    output_node_id   = character(0),
    output_node_name = character(0),
    output_pk_col    = character(0),
    output_pk_value  = character(0),
    join_type        = character(0),
    stringsAsFactors = FALSE
  )

  rows <- DBI::dbGetQuery(ctx$con,
    "SELECT rp.output_node_id, rp.output_pk_col, rp.output_pk_value,
            rp.join_type, n.name AS output_node_name
     FROM row_provenance rp
     JOIN lineage_nodes n ON n.node_id = rp.output_node_id
     WHERE rp.source_node_id = ? AND rp.source_pk_value = ?",
    params = list(as.character(source_node_id),
                  as.character(source_pk_value))
  )

  if (nrow(rows) == 0L) return(empty)
  rows
}

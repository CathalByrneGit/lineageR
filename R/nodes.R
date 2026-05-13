#' Register a data source node
#'
#' @param ctx A `LineageContext` from [lineage_context()].
#' @param name Human-readable name for this source.
#' @param description Optional free-text description.
#' @param connection_string Connection string or URI for the source (stored in
#'   metadata).
#' @param metadata Named list of additional metadata to store as JSON.
#' @return The generated `node_id` (character).
#' @export
lin_register_source <- function(ctx, name, description = NULL,
                                connection_string = NULL,
                                metadata = NULL) {
  meta <- metadata %||% list()
  if (!is.null(connection_string)) meta$connection_string <- connection_string

  node_id <- new_uuid()
  DBI::dbExecute(ctx$con,
    "INSERT INTO lineage_nodes
       (node_id, node_type, name, description, metadata_json, created_at)
     VALUES (?, 'source', ?, ?, ?, CURRENT_TIMESTAMP)",
    params = list(node_id, name, description,
                  jsonlite::toJSON(meta, auto_unbox = TRUE))
  )
  node_id
}

#' Register a transform node
#'
#' @param ctx A `LineageContext`.
#' @param name Name of the transform step.
#' @param description Optional description.
#' @param script_path Path to the script file (stored in metadata).
#' @param script_hash SHA-256 of the script file, used to detect changes.
#' @param language One of `"r"`, `"sql"`, `"python"`, `"other"`.
#' @param metadata Named list of additional metadata.
#' @return The generated `node_id`.
#' @export
lin_register_transform <- function(ctx, name, description = NULL,
                                   script_path = NULL,
                                   script_hash = NULL,
                                   language = c("r", "sql", "python", "other"),
                                   metadata = NULL) {
  language <- match.arg(language)
  meta <- metadata %||% list()
  if (!is.null(script_path)) meta$script_path <- script_path
  if (!is.null(script_hash)) meta$script_hash <- script_hash
  meta$language <- language

  node_id <- new_uuid()
  DBI::dbExecute(ctx$con,
    "INSERT INTO lineage_nodes
       (node_id, node_type, name, description, metadata_json, created_at)
     VALUES (?, 'transform', ?, ?, ?, CURRENT_TIMESTAMP)",
    params = list(node_id, name, description,
                  jsonlite::toJSON(meta, auto_unbox = TRUE))
  )
  node_id
}

#' Register a dataset node
#'
#' @param ctx A `LineageContext`.
#' @param name Name of the dataset.
#' @param table_name Actual DBI table name this dataset corresponds to.
#' @param description Optional description.
#' @param metadata Named list of additional metadata.
#' @return The generated `node_id`.
#' @export
lin_register_dataset <- function(ctx, name, table_name = NULL,
                                 description = NULL, metadata = NULL) {
  meta <- metadata %||% list()
  if (!is.null(table_name)) meta$table_name <- table_name

  node_id <- new_uuid()
  DBI::dbExecute(ctx$con,
    "INSERT INTO lineage_nodes
       (node_id, node_type, name, description, metadata_json, created_at)
     VALUES (?, 'dataset', ?, ?, ?, CURRENT_TIMESTAMP)",
    params = list(node_id, name, description,
                  jsonlite::toJSON(meta, auto_unbox = TRUE))
  )
  node_id
}

#' Register an object type node (or return the existing one)
#'
#' Idempotent: if a node with the same `object_type_id` already exists it is
#' returned unchanged. Optionally wires up a `backs` edge from a dataset.
#'
#' @param ctx A `LineageContext`.
#' @param object_type_id ID / name of the ontologySpecR object type.
#' @param dataset_node_id Optional `node_id` of the backing dataset. When
#'   provided, [lin_add_backing()] is called automatically.
#' @return The `node_id` (existing or newly created).
#' @export
lin_register_object_type <- function(ctx, object_type_id,
                                     dataset_node_id = NULL) {
  existing <- DBI::dbGetQuery(ctx$con,
    "SELECT node_id FROM lineage_nodes
     WHERE name = ? AND node_type = 'object_type'",
    params = list(object_type_id)
  )

  if (nrow(existing) > 0) {
    node_id <- existing$node_id[1]
  } else {
    node_id <- new_uuid()
    DBI::dbExecute(ctx$con,
      "INSERT INTO lineage_nodes (node_id, node_type, name, created_at)
       VALUES (?, 'object_type', ?, CURRENT_TIMESTAMP)",
      params = list(node_id, object_type_id)
    )
  }

  if (!is.null(dataset_node_id)) {
    lin_add_backing(ctx, dataset_node_id, node_id)
  }

  node_id
}

#' Sync all object types from a bundle as lineage nodes
#'
#' For each object type in `bundle$object_types`, upserts a node of type
#' `"object_type"`. If the object type carries a lineage extension
#' (`extensions$lineage$dataset_node_id`), the backing edge is wired
#' automatically. The call is idempotent.
#'
#' @param ctx A `LineageContext`.
#' @param bundle A list with an `object_types` element (a named list of object
#'   type definitions, each having at minimum an `id` field).
#' @return Invisibly, a character vector of `node_id`s.
#' @export
lin_sync_bundle <- function(ctx, bundle) {
  object_types <- bundle$object_types
  if (is.null(object_types)) {
    cli::cli_abort("{.arg bundle} must have an {.field object_types} element.")
  }

  ids <- character(length(object_types))
  for (i in seq_along(object_types)) {
    ot <- object_types[[i]]
    ot_id <- ot$id %||% names(object_types)[i]
    dataset_node_id <- ot$extensions$lineage$dataset_node_id
    ids[i] <- lin_register_object_type(ctx, ot_id,
                                       dataset_node_id = dataset_node_id)
  }

  invisible(ids)
}

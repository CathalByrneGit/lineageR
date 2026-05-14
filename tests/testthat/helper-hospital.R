# ── Hospital DuckDB fixture for cell-level lineage tests ──────────────────────

hospital_fixture <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  ctx <- lineage_context(con)

  DBI::dbExecute(con, "
    CREATE TABLE ehr_bed_stays_raw (
      stay_id  TEXT PRIMARY KEY,
      enc_id   TEXT,
      los_days INTEGER
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO ehr_bed_stays_raw VALUES
      ('stay_789', 'E-123', 5),
      ('stay_101', 'E-456', 2)
  ")

  DBI::dbExecute(con, "
    CREATE TABLE ehr_diagnoses_raw (
      dx_id          TEXT PRIMARY KEY,
      enc_id         TEXT,
      complete       BOOLEAN,
      pending_review BOOLEAN
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO ehr_diagnoses_raw VALUES
      ('dx_456', 'E-123', TRUE,  FALSE),
      ('dx_789', 'E-456', FALSE, TRUE)
  ")

  DBI::dbExecute(con, "
    CREATE TABLE encounters (
      enc_id             TEXT PRIMARY KEY,
      los_days           INTEGER,
      diagnosis_complete BOOLEAN,
      pending_review     BOOLEAN
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO encounters VALUES
      ('E-123', 5, TRUE,  FALSE),
      ('E-456', 2, FALSE, TRUE)
  ")

  # Lineage nodes
  beds_id      <- lin_register_source(ctx, "ehr_bed_stays_raw",
                    connection_string = "duckdb://memory")
  dx_id        <- lin_register_source(ctx, "ehr_diagnoses_raw",
                    connection_string = "duckdb://memory")
  enc_tform_id <- lin_register_transform(ctx, "build_encounters",
                    script_path = "pipelines/build_encounters.R")
  enc_ds_id    <- lin_register_dataset(ctx, "encounters",
                    table_name = "encounters")

  lin_add_input(ctx, enc_tform_id, beds_id)
  lin_add_input(ctx, enc_tform_id, dx_id)
  lin_add_output(ctx, enc_tform_id, enc_ds_id)

  # Expression lineage
  lin_record_expression(ctx,
    transform_node_id = enc_tform_id,
    output_node_id    = enc_ds_id,
    output_column     = "los_days",
    source_node_ids   = beds_id,
    source_columns    = "los_days",
    expression_text   = "MAX(bed_stays.los_days)"
  )
  lin_record_expression(ctx,
    transform_node_id = enc_tform_id,
    output_node_id    = enc_ds_id,
    output_column     = "diagnosis_complete",
    source_node_ids   = dx_id,
    source_columns    = "complete",
    expression_text   = "diagnoses.complete"
  )

  # Row provenance
  lin_record_row_provenance(ctx,
    output_node_id  = enc_ds_id,
    output_pk_col   = "enc_id",
    source_node_id  = beds_id,
    source_pk_col   = "stay_id",
    mapping = data.frame(
      output_pk_value = c("E-123", "E-456"),
      source_pk_value = c("stay_789", "stay_101"),
      stringsAsFactors = FALSE
    )
  )
  lin_record_row_provenance(ctx,
    output_node_id  = enc_ds_id,
    output_pk_col   = "enc_id",
    source_node_id  = dx_id,
    source_pk_col   = "dx_id",
    mapping = data.frame(
      output_pk_value = c("E-123", "E-456"),
      source_pk_value = c("dx_456", "dx_789"),
      stringsAsFactors = FALSE
    )
  )

  # Concept context — self-referential list built in two steps
  concept_ctx <- list(
    concepts = list(
      prolonged_stay = list(list(
        version    = 1L,
        expression = "los_days > 3 AND diagnosis_complete = TRUE AND NOT pending_review",
        table_name = "encounters",
        pk_col     = "enc_id",
        scope      = "Encounter"
      ))
    )
  )
  concept_ctx$get_concept <- function(concept_id, version = NULL) {
    defs <- concept_ctx$concepts[[concept_id]]
    if (is.null(version)) return(defs[[length(defs)]])
    idx <- which(vapply(defs, function(d)
      identical(d$version, as.integer(version)), logical(1)))
    if (length(idx) > 0L) defs[[idx[1L]]] else defs[[length(defs)]]
  }

  list(
    ctx          = ctx,
    con          = con,
    beds_id      = beds_id,
    dx_id        = dx_id,
    enc_tform_id = enc_tform_id,
    enc_ds_id    = enc_ds_id,
    concept_ctx  = concept_ctx
  )
}

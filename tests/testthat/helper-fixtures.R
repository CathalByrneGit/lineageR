make_ctx <- function() {
  con <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  lineage_context(con)
}

# Build the canonical 10-node, 8-edge test DAG described in AGENT.md.
#
# Chain: S3 -> ingest -> raw -> clean -> clean_a -> join -> joined -> Airport
#
# Isolated nodes: postgres_db, clean_b, FlightRoute
build_fixture <- function() {
  ctx <- make_ctx()

  # Sources
  s3_id  <- lin_register_source(ctx, "s3_bucket",    description = "Raw S3 bucket")
  pg_id  <- lin_register_source(ctx, "postgres_db",  description = "PostgreSQL database")

  # Transforms
  ingest_id <- lin_register_transform(ctx, "ingest", language = "r")
  clean_id  <- lin_register_transform(ctx, "clean",  language = "r")
  join_id   <- lin_register_transform(ctx, "join",   language = "sql")

  # Datasets
  raw_id     <- lin_register_dataset(ctx, "raw")
  clean_a_id <- lin_register_dataset(ctx, "clean_a")
  clean_b_id <- lin_register_dataset(ctx, "clean_b")
  joined_id  <- lin_register_dataset(ctx, "joined")

  # Object types
  airport_id <- lin_register_object_type(ctx, "Airport")
  route_id   <- lin_register_object_type(ctx, "FlightRoute")

  # Main chain edges
  lin_add_input(ctx,  ingest_id, s3_id)        # s3       -> ingest
  lin_add_output(ctx, ingest_id, raw_id)        # ingest   -> raw
  lin_add_input(ctx,  clean_id,  raw_id)        # raw      -> clean
  lin_add_output(ctx, clean_id,  clean_a_id)    # clean    -> clean_a
  lin_add_input(ctx,  join_id,   clean_a_id)    # clean_a  -> join
  lin_add_output(ctx, join_id,   joined_id)     # join     -> joined
  lin_add_backing(ctx, joined_id, airport_id)   # joined   -> Airport

  list(
    ctx        = ctx,
    s3_id      = s3_id,
    pg_id      = pg_id,
    ingest_id  = ingest_id,
    clean_id   = clean_id,
    join_id    = join_id,
    raw_id     = raw_id,
    clean_a_id = clean_a_id,
    clean_b_id = clean_b_id,
    joined_id  = joined_id,
    airport_id = airport_id,
    route_id   = route_id
  )
}

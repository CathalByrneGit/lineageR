<!-- README.md is generated from README.Rmd. Please edit README.Rmd, then knit with:
     devtools::build_readme()  or  knitr::knit("README.Rmd") -->

# lineageR

**Dataset Lineage and Provenance Tracking for Ontology-Backed Systems**

`lineageR` builds and queries a directed acyclic graph (DAG) that records
where data comes from, what transformed it, and what would break if it
changed — the same idea as Palantir Foundry's lineage view, but for the R
stack.

```
Source systems (databases, files, APIs)
        │  registered with lin_register_source()
        ▼
    Transforms (R / SQL / Python steps)
        │  registered with lin_register_transform()
        ▼
    Backing datasets ──► ontologySpecR object types
        │
        ▼
    lineageR DAG ──► provenance · impact analysis · freshness
```

## Installation

``` r
# Install from GitHub (requires remotes or pak)
remotes::install_github("CathalByrneGit/lineageR")
```

lineageR requires R ≥ 4.0 and depends on **DBI**, **igraph**, **dplyr**,
**rlang**, **jsonlite**, and **cli**. For interactive visualisation, install
the optional **visNetwork** or **ggraph** + **ggplot2** packages. Tests use
**duckdb**, which is the recommended DBI backend for local development.

## Quick start

``` r
library(lineageR)

# 1. Open a DBI connection (DuckDB shown; any DBI backend works)
con <- DBI::dbConnect(duckdb::duckdb(), "lineage.duckdb")
ctx <- lineage_context(con)   # creates schema tables if absent

# 2. Register data sources
s3_id <- lin_register_source(ctx, "s3_raw_flights",
  connection_string = "s3://my-bucket/raw/flights/")
pg_id <- lin_register_source(ctx, "postgres_schedules",
  connection_string = "postgresql://host/schedules")

# 3. Register transform steps
ingest_id <- lin_register_transform(ctx, "ingest_flights",
  script_path = "pipelines/ingest.R", language = "r")
clean_id  <- lin_register_transform(ctx, "clean_airports",
  script_path = "pipelines/clean.R",  language = "r")
join_id   <- lin_register_transform(ctx, "join_routes",
  script_path = "pipelines/join.sql", language = "sql")

# 4. Register output datasets
raw_id    <- lin_register_dataset(ctx, "flights_raw",    table_name = "flights_raw")
clean_id2 <- lin_register_dataset(ctx, "airports_clean", table_name = "airports_clean")
joined_id <- lin_register_dataset(ctx, "routes_joined",  table_name = "routes_joined")

# 5. Register ontology object types
airport_id <- lin_register_object_type(ctx, "Airport")
route_id   <- lin_register_object_type(ctx, "FlightRoute")

# 6. Wire the edges
lin_add_input(ctx,  ingest_id, s3_id)        # s3_raw_flights  ──► ingest_flights
lin_add_output(ctx, ingest_id, raw_id)        # ingest_flights  ──► flights_raw
lin_add_input(ctx,  clean_id,  raw_id)        # flights_raw     ──► clean_airports
lin_add_output(ctx, clean_id,  clean_id2)     # clean_airports  ──► airports_clean
lin_add_input(ctx,  join_id,   clean_id2)     # airports_clean  ──► join_routes
lin_add_input(ctx,  join_id,   pg_id)         # postgres        ──► join_routes
lin_add_output(ctx, join_id,   joined_id)     # join_routes     ──► routes_joined
lin_add_backing(ctx, clean_id2, airport_id)   # airports_clean  ──► Airport
lin_add_backing(ctx, joined_id, route_id)     # routes_joined   ──► FlightRoute
```

## Core concepts

### Node types

| Type          | What it represents                                    | Registered with              |
|---------------|-------------------------------------------------------|------------------------------|
| `source`      | External data origin (DB, file, API, …)              | `lin_register_source()`      |
| `transform`   | A computation step (R script, SQL query, pipeline)   | `lin_register_transform()`   |
| `dataset`     | A table or file produced by a transform               | `lin_register_dataset()`     |
| `object_type` | An ontologySpecR object type backed by a dataset      | `lin_register_object_type()` |

### Edge types

| Function            | Edge added               | Type         |
|---------------------|--------------------------|--------------|
| `lin_add_input()`   | `input ──► transform`    | `"consumes"` |
| `lin_add_output()`  | `transform ──► dataset`  | `"produces"` |
| `lin_add_backing()` | `dataset ──► object_type`| `"backs"`    |

Edges are **idempotent** — calling the same `lin_add_*()` twice is safe.
Adding an edge that would create a **cycle** aborts immediately with a
descriptive error.

## Querying the DAG

### Provenance — where did this data come from?

``` r
# All upstream ancestors of the Airport object type
lin_provenance(ctx, airport_id)
#>      node_id    node_type            name distance last_updated_at
#> 1 <uuid>        dataset   airports_clean        1            <NA>
#> 2 <uuid>        transform clean_airports        2            <NA>
#> 3 <uuid>        dataset   flights_raw           3            <NA>
#> 4 <uuid>        transform ingest_flights        4            <NA>
#> 5 <uuid>        source    s3_raw_flights        5            <NA>

# Direct parent only (depth = 1)
lin_provenance(ctx, airport_id, depth = 1)
#>      node_id    node_type          name distance last_updated_at
#> 1 <uuid>        dataset   airports_clean       1            <NA>
```

### Impact — what breaks if this source changes?

``` r
lin_impact(ctx, s3_id)
#>      node_id    node_type            name distance last_updated_at
#> 1 <uuid>        transform ingest_flights        1            <NA>
#> 2 <uuid>        dataset   flights_raw           2            <NA>
#> 3 <uuid>        transform clean_airports        3            <NA>
#> 4 <uuid>        dataset   airports_clean        4            <NA>
#> 5 <uuid>        transform join_routes           5            <NA>
#> 6 <uuid>        dataset   routes_joined         6            <NA>
#> 7 <uuid>        object_type Airport             7            <NA>
```

### Path — what is the route between two nodes?

``` r
lin_path(ctx, s3_id, airport_id)
#>      node_id    node_type            name         last_updated_at order
#> 1 <uuid>        source    s3_raw_flights                    <NA>     1
#> 2 <uuid>        transform ingest_flights                    <NA>     2
#> 3 <uuid>        dataset   flights_raw                       <NA>     3
#> 4 <uuid>        transform clean_airports                    <NA>     4
#> 5 <uuid>        dataset   airports_clean                    <NA>     5
#> 6 <uuid>        object_type Airport                         <NA>     6
```

Returns `NULL` when no directed path exists.

### Freshness — which nodes are stale?

A node is **stale** when any upstream ancestor has a `last_updated_at`
timestamp newer than the node's own timestamp.

``` r
lin_stale(ctx)
#>      node_id    name    node_type   stale_since         stale_upstream_node
#> 1 <uuid>        Airport object_type 2024-01-01 10:00:00 s3_raw_flights
```

Nodes without a `last_updated_at` are excluded — freshness cannot be
determined for them.

## Run logging

Record transform executions to keep `last_updated_at` current and maintain
an audit trail.

``` r
run_id <- lin_run_start(ctx, ingest_id)

tryCatch({
  # ... run the pipeline ...
  lin_run_complete(ctx, run_id, rows_produced = 48231L)
}, error = function(e) {
  lin_run_error(ctx, run_id, conditionMessage(e))
})

# View run history
lin_run_history(ctx, ingest_id, n = 10L)
#>    run_id started_at          completed_at        status rows_produced
#> 1 <uuid>  2024-06-01 08:00:00 2024-06-01 08:02:14 success         48231
```

`lin_run_complete()` automatically propagates `last_updated_at` to the
transform node and all its direct output dataset nodes.

## Visualisation

``` r
lin_plot(ctx)                              # full DAG
lin_plot(ctx, highlight_node_id = s3_id)  # highlight a node and its subtree
lin_plot(ctx, show_stale = TRUE)          # red border on stale nodes
lin_plot(ctx, layout = "fr")              # Fruchterman-Reingold layout
```

`lin_plot()` selects a rendering backend automatically:

1. **visNetwork** — interactive HTML widget (pan, zoom, hover tooltips)
2. **ggraph + ggplot2** — static ggplot object
3. **igraph** (base R) — always available fallback

Node colour key:

| Colour | Node type    |
|--------|--------------|
| Blue   | `source`     |
| Orange | `transform`  |
| Green  | `dataset`    |
| Purple | `object_type`|

## ontologySpecR bundle sync

If you already have an ontologySpecR bundle, sync all object types in one
call:

``` r
ctx <- lineage_context(con, bundle = my_bundle)
# or later:
lin_sync_bundle(ctx, my_bundle)
```

To auto-wire backing edges without manual `lin_add_backing()` calls, store
the dataset `node_id` in the lineage extension when defining the object type:

``` r
object_type("Airport",
  ...,
  extensions = list(
    lineage = list(dataset_node_id = airports_clean_node_id)
  )
)
```

## targets integration

``` r
# Import a targets pipeline — each target becomes a transform node
node_ids <- lin_from_targets(ctx, pipeline_path = "_targets.R")

# Graft in the raw source and final output dataset
lin_add_input(ctx, node_ids[["raw_data"]], s3_id)
lin_add_output(ctx, node_ids[["report"]], report_dataset_id)
```

See `vignette("targets-integration")` for a full walkthrough.

## Full API reference

### Context

| Function            | Description                                                  |
|---------------------|--------------------------------------------------------------|
| `lineage_context()` | Open a context; initialise schema; optionally sync a bundle  |

### Registering nodes

| Function                     | Node type                      |
|------------------------------|--------------------------------|
| `lin_register_source()`      | `source`                       |
| `lin_register_transform()`   | `transform`                    |
| `lin_register_dataset()`     | `dataset`                      |
| `lin_register_object_type()` | `object_type`                  |
| `lin_sync_bundle()`          | All object types from a bundle |

### Registering edges

| Function            | Edge                      |
|---------------------|---------------------------|
| `lin_add_input()`   | `input ──► transform`      |
| `lin_add_output()`  | `transform ──► dataset`    |
| `lin_add_backing()` | `dataset ──► object_type`  |

### Querying

| Function           | Returns                                               |
|--------------------|-------------------------------------------------------|
| `lin_graph()`      | Full DAG as an `igraph` object                        |
| `lin_provenance()` | Data frame of upstream ancestors (with `depth` limit) |
| `lin_impact()`     | Data frame of downstream descendants                  |
| `lin_stale()`      | Data frame of nodes stale relative to their upstream  |
| `lin_path()`       | Ordered data frame of nodes on the shortest path      |

### Run logging

| Function             | Effect                                                |
|----------------------|-------------------------------------------------------|
| `lin_run_start()`    | Insert a `"running"` row; return `run_id`             |
| `lin_run_complete()` | Set `"success"`; propagate `last_updated_at` to outputs |
| `lin_run_error()`    | Set `"error"` with message                            |
| `lin_run_history()`  | Recent run rows for a transform                       |

### Visualisation & integration

| Function             | Description                                    |
|----------------------|------------------------------------------------|
| `lin_plot()`         | Interactive or static DAG plot                 |
| `lin_from_targets()` | Import a targets pipeline as transform nodes   |

## Database schema

lineageR persists everything in three tables created automatically by
`lineage_context()` (safe to call multiple times):

| Table               | Contents                                              |
|---------------------|-------------------------------------------------------|
| `lineage_nodes`     | One row per node (`source` / `transform` / `dataset` / `object_type`) |
| `lineage_edges`     | One row per directed edge                             |
| `lineage_run_log`   | One row per transform execution                       |

Any DBI-compatible backend works: DuckDB, SQLite, PostgreSQL, and others.

## License

MIT © lineageR Authors

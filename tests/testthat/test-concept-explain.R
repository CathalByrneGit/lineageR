test_that("lin_explain_concept returns a CellExplanation object", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  expl <- lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-123")
  expect_s3_class(expl, "CellExplanation")
})

test_that("lin_explain_concept returns TRUE overall for E-123", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  expl <- lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-123")
  expect_true(isTRUE(expl$overall_result))
})

test_that("lin_explain_concept returns FALSE overall for E-456", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  expl <- lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-456")
  expect_false(isTRUE(expl$overall_result))
})

test_that("lin_explain_concept produces 3 sub-expressions for a 3-clause AND", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  expl <- lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-123")
  expect_length(expl$sub_expressions, 3L)
})

test_that("each sub-expression has expr, result, and source_lineage fields", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  expl <- lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-123")
  for (s in expl$sub_expressions) {
    expect_true("expr"           %in% names(s))
    expect_true("result"         %in% names(s))
    expect_true("source_lineage" %in% names(s))
  }
})

test_that("lin_explain_concept stores result in concept_cell_explanations", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-123")

  n <- DBI::dbGetQuery(h$ctx$con,
    "SELECT COUNT(*) AS n FROM concept_cell_explanations
     WHERE concept_id = 'prolonged_stay' AND object_pk_value = 'E-123'")$n
  expect_equal(n, 1L)
})

test_that("concept_id, scope, object_pk_value are set correctly on the result", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  expl <- lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-123")
  expect_equal(expl$concept_id,      "prolonged_stay")
  expect_equal(expl$scope,           "Encounter")
  expect_equal(expl$object_pk_value, "E-123")
})

test_that("lin_explain_concept_batch returns named list of length 2", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  results <- lin_explain_concept_batch(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", c("E-123", "E-456"))
  expect_length(results, 2L)
  expect_equal(names(results), c("E-123", "E-456"))
})

test_that("lin_explain_concept_batch: E-123 TRUE, E-456 FALSE", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  results <- lin_explain_concept_batch(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", c("E-123", "E-456"))
  expect_true(isTRUE(results[["E-123"]]$overall_result))
  expect_false(isTRUE(results[["E-456"]]$overall_result))
})

test_that("print.CellExplanation outputs without error", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  expl <- lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-123")
  expect_no_error(print(expl))
})

test_that("format.CellExplanation returns a character string containing concept_id", {
  h <- hospital_fixture()
  on.exit(DBI::dbDisconnect(h$con, shutdown = TRUE))

  expl <- lin_explain_concept(h$ctx, h$concept_ctx,
    "prolonged_stay", "Encounter", "E-123")
  out <- format(expl)
  expect_type(out, "character")
  expect_match(out, "prolonged_stay")
  expect_match(out, "E-123")
})

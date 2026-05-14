test_that("simple AND expression produces AND node with 3 children", {
  tree <- decompose_sql_boolean("a > 3 AND b = TRUE AND NOT c")
  expect_equal(tree$op, "AND")
  expect_length(tree$children, 3L)

  ops <- vapply(tree$children, `[[`, character(1), "op")
  expect_true("LEAF" %in% ops)
  expect_equal(tree$children[[3L]]$op, "NOT")
})

test_that("LEAF children carry the correct expression text", {
  tree   <- decompose_sql_boolean("a > 3 AND b = TRUE AND NOT c")
  leaves <- Filter(function(x) x$op == "LEAF", tree$children)
  exprs  <- vapply(leaves, `[[`, character(1), "expr")
  expect_true("a > 3"    %in% exprs)
  expect_true("b = TRUE" %in% exprs)
})

test_that("parenthesized OR inside AND forms a nested tree", {
  tree <- decompose_sql_boolean("(a > 3 OR b < 2) AND c = TRUE")
  expect_equal(tree$op, "AND")
  expect_length(tree$children, 2L)

  or_child <- tree$children[[1L]]
  expect_equal(or_child$op, "OR")
  expect_length(or_child$children, 2L)
})

test_that("NOT prefix creates a NOT node with one LEAF child", {
  tree <- decompose_sql_boolean("NOT pending_review")
  expect_equal(tree$op, "NOT")
  expect_length(tree$children, 1L)
  expect_equal(tree$children[[1L]]$op,   "LEAF")
  expect_equal(tree$children[[1L]]$expr, "pending_review")
})

test_that("single expression returns a LEAF node", {
  tree <- decompose_sql_boolean("los_days > 3")
  expect_equal(tree$op,   "LEAF")
  expect_equal(tree$expr, "los_days > 3")
})

test_that("AND inside single quotes is not a split point", {
  tree <- decompose_sql_boolean("name = 'Bob AND Alice'")
  expect_equal(tree$op,   "LEAF")
  expect_equal(tree$expr, "name = 'Bob AND Alice'")
})

test_that("OR expression splits into two LEAF children", {
  tree <- decompose_sql_boolean("x = 1 OR y = 2")
  expect_equal(tree$op, "OR")
  expect_length(tree$children, 2L)
})

test_that("parse_sql_column_refs extracts column identifiers", {
  cols <- parse_sql_column_refs("los_days > 3 AND diagnosis_complete = TRUE")
  expect_true("los_days"           %in% cols)
  expect_true("diagnosis_complete" %in% cols)
})

test_that("parse_sql_column_refs excludes SQL keywords", {
  cols <- parse_sql_column_refs("los_days > 3 AND diagnosis_complete = TRUE")
  expect_false(any(toupper(cols) %in% c("AND", "TRUE", "FALSE")))
})

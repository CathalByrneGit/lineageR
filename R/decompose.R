# ── SQL boolean decomposer ────────────────────────────────────────────────────

#' Decompose a SQL boolean expression into a tree
#'
#' Splits the top-level AND / OR connectives of `sql_expr` into a nested list
#' tree. Each node has `op` (one of `"AND"`, `"OR"`, `"NOT"`, `"LEAF"`) and
#' either `children` (list of sub-trees) or `expr` (the leaf string).
#'
#' The parser respects parentheses and single-quoted string literals, so
#' `a > 'x AND y'` is treated as a single leaf.
#'
#' @param sql_expr Character scalar. A SQL boolean expression.
#' @return A named list with `op` and either `expr` (LEAF) or `children`.
#' @export
decompose_sql_boolean <- function(sql_expr) {
  .decompose_expr(trimws(as.character(sql_expr)))
}

.decompose_expr <- function(expr) {
  expr <- trimws(.strip_outer_parens(trimws(expr)))

  and_parts <- .split_top_level_op(expr, "AND")
  if (length(and_parts) > 1L) {
    return(list(op = "AND", children = lapply(and_parts, .decompose_expr)))
  }

  or_parts <- .split_top_level_op(expr, "OR")
  if (length(or_parts) > 1L) {
    return(list(op = "OR", children = lapply(or_parts, .decompose_expr)))
  }

  if (grepl("^NOT\\s+", trimws(expr), ignore.case = TRUE)) {
    inner <- trimws(sub("^NOT\\s+", "", trimws(expr), ignore.case = TRUE))
    return(list(op = "NOT", children = list(.decompose_expr(inner))))
  }

  list(op = "LEAF", expr = expr)
}

.strip_outer_parens <- function(expr) {
  if (nchar(expr) < 2L || !startsWith(expr, "(") || !endsWith(expr, ")")) {
    return(expr)
  }
  if (.is_fully_parenthesized(expr)) {
    return(trimws(substr(expr, 2L, nchar(expr) - 1L)))
  }
  expr
}

.is_fully_parenthesized <- function(expr) {
  if (!startsWith(expr, "(")) return(FALSE)
  depth  <- 0L
  in_str <- FALSE
  chars  <- strsplit(expr, "")[[1L]]
  n      <- length(chars)
  for (i in seq_len(n)) {
    ch <- chars[i]
    if (in_str) {
      if (ch == "'") in_str <- FALSE
      next
    }
    if (ch == "'") { in_str <- TRUE; next }
    if (ch == "(") depth <- depth + 1L
    if (ch == ")") {
      depth <- depth - 1L
      if (depth == 0L && i < n) return(FALSE)
    }
  }
  TRUE
}

# Split on a top-level keyword operator (AND / OR).
# Returns a character vector of the parts, or the original expr as length-1.
.split_top_level_op <- function(expr, operator) {
  op_len  <- nchar(operator)
  expr_u  <- toupper(expr)
  chars   <- strsplit(expr, "")[[1L]]
  n       <- length(chars)
  depth   <- 0L
  in_str  <- FALSE
  parts   <- character(0)
  start   <- 1L
  i       <- 1L

  while (i <= n) {
    ch <- chars[i]

    if (in_str) {
      if (ch == "'") in_str <- FALSE
      i <- i + 1L; next
    }
    if (ch == "'") { in_str <- TRUE; i <- i + 1L; next }
    if (ch == "(") { depth <- depth + 1L; i <- i + 1L; next }
    if (ch == ")") { depth <- depth - 1L; i <- i + 1L; next }

    if (depth == 0L && (i + op_len - 1L) <= n) {
      candidate <- substr(expr_u, i, i + op_len - 1L)
      if (candidate == operator) {
        before_ok <- (i == 1L) || !grepl("[A-Za-z0-9_]", chars[i - 1L])
        after_ok  <- (i + op_len > n) ||
                     !grepl("[A-Za-z0-9_]", chars[i + op_len])
        if (before_ok && after_ok) {
          parts <- c(parts, trimws(substr(expr, start, i - 1L)))
          start <- i + op_len
          i     <- start
          next
        }
      }
    }
    i <- i + 1L
  }

  if (length(parts) == 0L) return(expr)
  c(parts, trimws(substr(expr, start, n)))
}

# Split on top-level commas (no word-boundary checks needed).
.split_top_level_comma <- function(expr) {
  chars  <- strsplit(expr, "")[[1L]]
  n      <- length(chars)
  depth  <- 0L
  in_str <- FALSE
  parts  <- character(0)
  start  <- 1L

  for (i in seq_len(n)) {
    ch <- chars[i]
    if (in_str) { if (ch == "'") in_str <- FALSE; next }
    if (ch == "'") { in_str <- TRUE; next }
    if (ch == "(") { depth <- depth + 1L; next }
    if (ch == ")") { depth <- depth - 1L; next }
    if (depth == 0L && ch == ",") {
      parts <- c(parts, trimws(substr(expr, start, i - 1L)))
      start <- i + 1L
    }
  }
  c(parts, trimws(substr(expr, start, n)))
}

# Collect top-level sub-conditions from a tree (used by lin_explain_concept).
# For an AND/OR tree, returns the direct children as expression strings.
# For NOT, prepends "NOT ". For LEAF, returns the expr as-is.
.collect_sub_conditions <- function(tree) {
  if (tree$op %in% c("AND", "OR")) {
    unlist(lapply(tree$children, .sub_condition_expr), use.names = FALSE)
  } else {
    .sub_condition_expr(tree)
  }
}

.sub_condition_expr <- function(tree) {
  if (tree$op == "LEAF") return(tree$expr)
  if (tree$op == "NOT") {
    inner <- .sub_condition_expr(tree$children[[1L]])
    return(paste("NOT", inner))
  }
  .collect_sub_conditions(tree)
}

#' Extract column references from a SQL expression
#'
#' Returns a character vector of identifiers that look like column names
#' (bare words that are not SQL keywords or function names).
#'
#' @param sql Character scalar.
#' @return Character vector of column-like identifiers (unique, original case).
#' @export
parse_sql_column_refs <- function(sql) {
  keywords <- c(
    "SELECT", "FROM", "WHERE", "AND", "OR", "NOT", "IN", "IS", "NULL",
    "TRUE", "FALSE", "LIKE", "BETWEEN", "CASE", "WHEN", "THEN", "ELSE",
    "END", "AS", "ON", "JOIN", "LEFT", "RIGHT", "INNER", "OUTER",
    "GROUP", "BY", "HAVING", "ORDER", "LIMIT", "DISTINCT", "COUNT",
    "SUM", "AVG", "MIN", "MAX", "COALESCE", "CAST", "OVER", "PARTITION",
    "FILTER", "WHERE", "BOOLEAN", "INTEGER", "VARCHAR", "TEXT"
  )
  tokens <- regmatches(sql, gregexpr("\\b[A-Za-z_][A-Za-z0-9_]*\\b", sql))[[1L]]
  unique(tokens[!toupper(tokens) %in% keywords])
}

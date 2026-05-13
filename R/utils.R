`%||%` <- function(x, y) if (is.null(x)) y else x

new_uuid <- function() {
  hex_chars <- c(as.character(0:9), letters[1:6])
  rand_hex <- function(n) paste(sample(hex_chars, n, replace = TRUE), collapse = "")
  paste(
    rand_hex(8), rand_hex(4),
    paste0("4", rand_hex(3)),
    paste0(sample(c("8", "9", "a", "b"), 1), rand_hex(3)),
    rand_hex(12),
    sep = "-"
  )
}

.as_posixct_safe <- function(x) {
  if (is.null(x) || all(is.na(x))) return(as.POSIXct(NA))
  if (inherits(x, "POSIXct")) return(x)
  as.POSIXct(x, tz = "UTC")
}

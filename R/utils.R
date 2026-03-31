# From dplyr
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

# Check if a scalar value is NULL or NA
null_or_na <- function(x) {
  is.null(x) || rlang::is_na(x)
}

# Return the first scalar value in `...` that is not NULL or NA.
# Need to handle NA because NA values may be passed via data.frame col vals in
# as_acc_calibration()
first_valid <- function(...) {
  for (v in list(...)) {
    if (!is.null(v) && !rlang::is_na(v)) return(v)
  }
  NULL
}

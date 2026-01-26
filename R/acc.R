#'  Create a `acc` vector
#'
#' @param bursts a list of matrices
#'
#' @param frequency The frequency of the acceleration recordings. Either the same length of `bursts` or it will be recycled
#' @param start Start time of the burst, in POSIXct format
#'
#' @export
acc <- function(bursts = list(), 
                frequency = units::set_units(double(), "Hz"),
                start = NULL) {
  bursts <- new_acc_list(bursts)
  n <- vec_size(bursts)
  
  frequency <- vec_recycle(frequency, n)
  
  start <- start %||% as.POSIXct(NA_real_, tz = "UTC")
  start <- vec_recycle(start, n)
  
  new_acc(bursts, frequency, start)
}

new_acc <- function(bursts = new_acc_list(list()), 
                    frequency = units::set_units(double(), "Hz"),
                    start = as.POSIXct(double(), tz = "UTC")) {
  assertthat::assert_that(inherits(start, "POSIXct"))
  
  new_rcrd(
    list(bursts = bursts, frequency = frequency, start = start),
    class = "acc"
  )
}

acc_list <- function(x) {
  new_acc_list(x)
}

new_acc_list <- function(x) {
  assertthat::assert_that(all(unlist(lapply(x, \(y) is.null(y) || !is.null(colnames(y))))))
  new_list_of(x, ptype = matrix(numeric()), class = "acc_list")
}
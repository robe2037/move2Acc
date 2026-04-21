#'  Create a `acc` vector
#'
#' @param bursts a list of matrices
#' @param frequency The frequency of the acceleration recordings. Either the
#'   same length of `bursts` or it will be recycled. If no units are specified,
#'   the frequency is assumed to be in Hz.
#' @param start Start time of the burst, in POSIXct format
#'
#' @export
acc <- function(bursts = list(),
                frequency = units::set_units(double(), "Hz"),
                start = NULL) {
  sensor_rcrd("acc", bursts = bursts, frequency = frequency, start = start)
}

new_acc <- function(bursts = new_acc_list(list()),
                    frequency = units::set_units(double(), "Hz"),
                    start = as.POSIXct(double(), tz = "UTC")) {
  new_sensor_rcrd("acc", bursts = bursts, frequency = frequency, start = start)
}

acc_list <- function(x) {
  sensor_list(x, "acc")
}

new_acc_list <- function(x) {
  new_sensor_list(x, "acc")
}

#' @export
#' @rdname explore-functions
is_acc <- function(x) {
  inherits(x, "acc")
}

#' @export
vec_ptype2.acc.acc <- function(x, y, ...) sensor_ptype2(x, y, ...)

#' @export
vec_cast.acc.acc <- function(x, to, ...) sensor_cast(x, to, ...)

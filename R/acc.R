#'  Create a `acc` vector
#'
#' @param bursts a list of matrices
#' @param frequency The sampling frequency of the recordings in `bursts`. Either the
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

#' @export
#' @rdname explore-functions
is_acc <- function(x) {
  inherits(x, "acc")
}

#' @export
vec_ptype2.acc.acc <- function(x, y, ...) sensor_ptype2(x, y, ...)

#' @export
vec_cast.acc.acc <- function(x, to, ...) sensor_cast(x, to, ...)

#'  Create a `mag` vector
#'
#' @inheritParams acc
#' @param bursts a list of matrices
#' @param frequency The frequency of the magnetometer recordings. Either the
#'   same length of `bursts` or it will be recycled. If no units are specified,
#'   the frequency is assumed to be in Hz.
#'
#' @export
mag <- function(bursts = list(),
                frequency = units::set_units(double(), "Hz"),
                start = NULL) {
  sensor_rcrd("mag", bursts = bursts, frequency = frequency, start = start)
}

new_mag <- function(bursts = new_mag_list(list()),
                    frequency = units::set_units(double(), "Hz"),
                    start = as.POSIXct(double(), tz = "UTC")) {
  new_sensor_rcrd("mag", bursts = bursts, frequency = frequency, start = start)
}

mag_list <- function(x) {
  sensor_list(x, "mag")
}

new_mag_list <- function(x) {
  new_sensor_list(x, "mag")
}

#' @export
#' @rdname explore-functions
is_mag <- function(x) {
  inherits(x, "mag")
}

#' @export
vec_ptype2.mag.mag <- function(x, y, ...) sensor_ptype2(x, y, ...)

#' @export
vec_cast.mag.mag <- function(x, to, ...) sensor_cast(x, to, ...)

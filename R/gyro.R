# Concrete `gyro` subclass of `imu`. All shared record-level logic
# lives in R/imu.R; this file only holds the gyro-specific constructor,
# predicate, and S3 dispatch wrappers.

#' Create a `gyro` vector
#'
#' @inheritParams acc
#' @param frequency The frequency of the gyroscope recordings. Either the
#'   same length of `bursts` or it will be recycled. If no units are specified,
#'   the frequency is assumed to be in Hz.
#'
#' @export
gyro <- function(bursts = list(),
                 frequency = units::set_units(double(), "Hz"),
                 start = NULL) {
  imu("gyro", bursts = bursts, frequency = frequency, start = start)
}

#' @export
#' @rdname explore-functions
is_gyro <- function(x) {
  inherits(x, "gyro")
}

#' @export
vec_ptype2.gyro.gyro <- function(x, y, ...) imu_ptype2(x, y, ...)

#' @export
vec_cast.gyro.gyro <- function(x, to, ...) imu_cast(x, to, ...)

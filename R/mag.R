#'  Create a `mag` vector
#'
#' @inheritParams acc
#'
#' @export
mag <- function(bursts = list(),
                frequency = units::set_units(double(), "Hz"),
                start = NULL) {
  sensor_rcrd("mag", bursts = bursts, frequency = frequency, start = start)
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

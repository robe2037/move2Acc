#' Convert to gyro
#'
#' In many cases the `as_gyro` function will directly create a gyroscope
#' vector from input data
#'
#' @inheritParams as_acc
#' @param x A `move2` containing gyroscope data. Most of the time this will be
#'   either loaded from disk using [move2::mt_read] or downloaded using
#'   [move2::movebank_download_study].
#' @param colset A `gyro_colset` object or list of `gyro_colset` objects
#'   specifying the columns of `x` that contain gyroscope data. By default,
#'   constructs bursts for all column sets that are detected in `x` that also
#'   contain data (see [active_gyro_colsets()]).
#'
#'   Several common colsets are listed under [valid_gyro_colsets()]. To
#'   specify a custom set of columns, use [gyro_colset()].
#'
#' @details The resulting vector will be as long as the input. This means it
#' can, for example, be added as a column to a `data.frame`. For some tags
#' this means `NA` values are inserted when one burst is stored over multiple
#' rows of a `data.frame`.
#'
#' @export
as_gyro <- function(x, ...) {
  UseMethod("as_gyro")
}

#' @rdname as_gyro
#' @export
as_gyro.default <- function(x, ...) {
  vctrs::vec_cast(x, new_sensor_rcrd("gyro"))
}

#' @rdname as_gyro
#' @export
as_gyro.move2 <- function(x, colset = NULL, min_freq = 1, merge_continuous = TRUE, drop = TRUE, ...) {
  as_sensor(
    x,
    sensor = "gyro",
    colset = colset,
    min_freq = min_freq,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}

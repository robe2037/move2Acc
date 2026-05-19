#' Convert to mag
#'
#' In many cases the `as_mag` function will directly create a magnetometer
#' vector from input data
#'
#' @inheritParams as_acc
#' @param x A `move2` containing magnetometer data. Most of the time this will be
#'   either loaded from disk using [move2::mt_read] or downloaded using
#'   [move2::movebank_download_study].
#' @param colset An `imu_colset` object or list of `imu_colset` objects
#'   specifying the columns of `x` that contain magnetometer data. By default,
#'   constructs bursts for all column sets that are detected in `x` that also
#'   contain data (see [active_mag_colsets()]).
#'
#'   Several common colsets are listed under [valid_mag_colsets()]. To
#'   specify a custom set of columns, use [imu_colset()].
#'
#' @details The resulting vector will be as long as the input. This means it
#' can, for example, be added as a column to a `data.frame`. For some tags
#' this means `NA` values are inserted when one burst is stored over multiple
#' rows of a `data.frame`.
#'
#' @export
as_mag <- function(x, ...) {
  UseMethod("as_mag")
}

#' @rdname as_mag
#' @export
as_mag.default <- function(x, ...) {
  vctrs::vec_cast(x, new_imu("mag"))
}

#' @rdname as_mag
#' @export
as_mag.move2 <- function(x, colset = NULL, min_freq = 1, merge_continuous = TRUE, drop = TRUE, ...) {
  as_imu(
    x,
    sensor = "mag",
    colset = colset,
    min_freq = min_freq,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}

#' Convert to acc
#'
#' In many cases the `as_acc` function will directly create an acceleration
#' vector from input data
#'
#' @param x A `move2` containing acceleration data as collected by EOBS,
#'   Ornitela, or similar tracking devices. Most of the time this will be
#'   either loaded from disk using [move2::mt_read] or downloaded using
#'   [move2::movebank_download_study].
#' @param colset An `acc_colset` object or list of `acc_colset` objects
#'   specifying the columns of `x` that contain acceleration data. By default,
#'   constructs bursts for all column sets that are detected in `x` that also
#'   contain data (see [active_acc_colsets()]).
#'
#'   Several common colsets are listed under [valid_acc_colsets()]. To
#'   specify a custom set of columns, use [acc_colset()].
#' @param min_freq Numeric value indicating the
#'   minimum allowable within-burst data collection frequency when identifying
#'   bursts in long-format data. Any two adjacent timestamps
#'   that fall outside of the period defined by this frequency will be split
#'   into separate bursts. If no units are provided, this value is assumed to
#'   be in Hz.
#'
#'   Ignored if data are already in predefined bursts.
#' @param merge_continuous Logical value indicating whether to merge
#'   adjacent bursts. Two adjacent bursts can be merged if the
#'   first burst ends at the same time that the second starts and the
#'   burst frequency is identical between the two. This is useful for
#'   processing continuous data that have been stored in chunks
#'   split at regular intervals.
#' @param drop Logical indicating whether empty bursts should
#'   be dropped from the output. If `drop = FALSE`, then the length of the
#'   output will match the number of rows in the input data `x` and
#'   bursts will be stored at the index location corresponding to the start time
#'   of the burst.
#' @param ... currently not used
#'
#' @details The resulting vector will be as long as the input. This means it
#' can, for example, be added as a column to a `data.frame`. For some tags
#' this means `NA` values are inserted when one burst is stored over multiple
#' rows of a `data.frame`.
#'
#' @export
as_acc <- function(x, ...) {
  UseMethod("as_acc")
}

#' @rdname as_acc
#' @export
as_acc.default <- function(x, ...) {
  vctrs::vec_cast(x, new_imu("acc"))
}

#' @rdname as_acc
#' @export
as_acc.move2 <- function(x, colset = NULL, min_freq = 1, merge_continuous = TRUE, drop = TRUE, ...) {
  as_imu(
    x,
    sensor = "acc",
    colset = colset,
    min_freq = min_freq,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}

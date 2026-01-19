#' Convert to acc
#'
#' In many cases the `as_acc` function will directly create an acceleration vector from input data
#'
#' @param x A `move2` containing acceleration data as collected by EOBS or ornitella tracking devices. Most of the time
#'   this will be either loaded from disk using [move2::mt_read] or downloaded using [move2::movebank_download_study].
#'
#' @param ... currently not used
#'
#' @details The resulting vector will be as long as the input. This means it can, for example, be added as a column to a
#' `data.frame`. For some tags this means `NA` values are inserted when one burst is stored over multiple rows of a
#' `data.frame`.
#'
#'
#' @export

as_acc <- function(x, ...) {
  UseMethod("as_acc")
}
#' @export
as_acc.default <- function(x, ...) {
  vctrs::vec_cast(x, new_acc())
}

#' @export
as_acc.move2 <- function(x, ...) {
  if (all(c("tilt_x", "tilt_y", "tilt_z", "start_timestamp") %in% colnames(x))) {
    acc <- as_acc_move2_ornitella(x, ...)
  } else if (has_eobs_burst_cols(x)) {
    acc <- as_acc_move2_eobs(x, ...)
  } else if (has_acc_burst_cols(x)) {
    acc <- as_acc_move2_burst(x)
  } else {
    stop("No acc conversion implemented")
  }
  
  acc
}

as_acc_move2_ornitella <- function(x, ...) {
  assertthat::assert_that(units(x$tilt_x)==units(x$tilt_y))
  assertthat::assert_that(units(x$tilt_x)==units(x$tilt_z))
  assertthat::assert_that(!any(unlist(lapply(lapply(split( move2::mt_track_id(x),x$start_timestamp),unique), length))!=1))
  m<-as.matrix(data.frame(x)[, c("tilt_x", "tilt_y", "tilt_z")])* units::as_units(units::deparse_unit(x$tilt_x))
  lst<-purrr::map(split(seq_len(nrow(m)), x$start_timestamp), ~m[.x,])

  frq <- do.call(c, lapply(
      lapply(
       diffs<- lapply(
          split(
            move2::mt_time(x),
            x$start_timestamp
          ), diff
        ), units::as_units
      ),
      \(x) mean(1 / x)
  ))
  assertthat::assert_that( all(unlist(diffs)>0))
  acc <- vec_rep(new_acc(list(NULL), units::set_units(NA, "Hz")), nrow(x))
  s <- !duplicated(x$start_timestamp) & !is.na(x$start_timestamp)
  acc[s] <- new_acc(lst, frq)
  acc
}

as_acc_burst <- function(acc, axes, freq, start_timestamp = NULL) {
  colnms <- strsplit(as.character(axes), "")
  n_axis <- nchar(as.character(axes))
  mlist <- strsplit(acc, " ") |> lapply(as.integer)
  i <- !is.na(n_axis)
  mlist[!i] <- list(NULL)
  mlist[i] <- mapply(matrix, mlist[i], ncol = n_axis[i], MoreArgs = list(byrow = TRUE), SIMPLIFY = FALSE)
  mlist[i] <- mapply("colnames<-", mlist[i], colnms[i], SIMPLIFY = FALSE)
  new_acc(mlist, frequency = freq)
}

as_acc_move2_eobs <- function(x, ...) {
  assertthat::assert_that(has_eobs_burst_cols(x))
  
  as_acc_burst(
    x[["eobs_accelerations_raw"]],
    x[["eobs_acceleration_axes"]],
    x[["eobs_acceleration_sampling_frequency_per_axis"]]
  )
}

as_acc_move2_burst <- function(x, ...) {
  assertthat::assert_that(has_acc_burst_cols(x))
  
  as_acc_burst(
    x[["accelerations_raw"]],
    x[["acceleration_axes"]],
    x[["acceleration_sampling_frequency_per_axis"]]
  )
}

has_eobs_burst_cols <- function(x) { # Suppose we will also need to check which of these columns actually has data...
  all(eobs_burst_cols() %in% colnames(x))
}

has_acc_burst_cols <- function(x) {
  all(acc_burst_cols() %in% colnames(x))
}

has_acc_raw_xyz_cols <- function(x) {
  any(acc_raw_xyz_cols() %in% colnames(x))
}

has_acc_xyz_cols <- function(x) {
  any(acc_xyz_cols() %in% colnames(x))
}

eobs_burst_cols <- function() {
  c(
    "eobs_acceleration_axes", 
    "eobs_acceleration_sampling_frequency_per_axis", 
    "eobs_accelerations_raw"
  )
}

acc_burst_cols <- function() {
  c(
    "acceleration_axes",
    "acceleration_sampling_frequency_per_axis",
    "accelerations_raw"
  )
}

acc_raw_xyz_cols <- function() {
  c(
    "acceleration_raw_x", 
    "acceleration_raw_y", 
    "acceleration_raw_z"
  )
}

acc_xyz_cols <- function() {
  c(
    "acceleration_x", 
    "acceleration_y", 
    "acceleration_z"
  )
}

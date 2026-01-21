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
as_acc.move2 <- function(x, tolerance = 0.5, ...) {
  if (has_acc_eobs_cols(x)) {
    acc <- as_acc_move2_eobs(x, ...)
  } else if (has_acc_burst_cols(x)) {
    acc <- as_acc_move2_burst(x, ...)
  } else if (has_acc_xyz_cols(x)) {
    acc <- as_acc_move2_xyz(x, tolerance = tolerance, ...)
  } else if (has_acc_raw_xyz_cols(x)) {
    acc <- as_acc_move2_raw_xyz(x, tolerance = tolerance, ...)
  } else if (has_acc_tilt_cols(x)) {
    acc <- as_acc_move2_tilt(x, tolerance = tolerance, ...)
  } else {
    stop("No acc conversion implemented")
  }
  
  acc
}

as_acc_long <- function(x, tolerance = 1, acc_cols = active_acc_cols(x), ...) {
  assertthat::assert_that(is_valid_acc_colset(acc_cols))
  assert_matched_acc_units(x, acc_cols)
  
  m <- as.matrix(data.frame(x)[, acc_cols])
  
  # TODO: may want a safer way to handle units. Some acc will have units, others not
  if (inherits(x[[acc_cols[[1]]]], "units")) {
    m <- m * units::as_units(units::deparse_unit(x[[acc_cols[[1]]]]))
  }
  
  # Generate vector of ids for each distinct burst based on sequential
  # timestamps within a given temporal tolerance
  ts_grps <- group_timestamps(x, tolerance = tolerance)
  
  # Split all rows with acc data into burst groups based on timestamp groups
  idx <- unname(split(which_acc_vals(x), ts_grps))
  
  # Extract records for each burst into a separate matrix
  acc_lst <- lapply(idx, function(i) m[i, , drop = FALSE])
  
  # Calculate mean frequency for each burst
  freq <- unname(unlist(
    lapply(
      split(move2::mt_time(x[which_acc_vals(x), ]), ts_grps), 
      function(y) {
        ifelse(length(y) <= 1, NA, mean(1 / units::as_units(diff(y))))
      }
    )
  ))
  
  # Attach acc bursts to index of the first record that belongs to that burst
  acc <- vec_rep(new_acc(list(NULL), units::set_units(NA, "Hz")), nrow(x))
  s <- sapply(idx, function(x) x[1]) # first index of each ts group
  
  if (length(s) > 0) {
    acc[s] <- new_acc(acc_lst, units::as_units(freq, "Hz"))
  }
  
  acc
}

as_acc_move2_raw_xyz <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_raw_xyz_cols(x))
  as_acc_long(x, tolerance = tolerance, acc_cols = acc_raw_xyz_cols())
}

as_acc_move2_xyz <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_xyz_cols(x))
  as_acc_long(x, tolerance = tolerance, acc_cols = acc_xyz_cols())
}

# TODO: decide whether tilt is supported? It seems to co-occur with raw xyz cols
as_acc_move2_tilt <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_tilt_cols(x))
  as_acc_long(x, tolerance = tolerance, acc_cols = acc_tilt_cols())
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
  assertthat::assert_that(has_acc_eobs_cols(x))
  
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

which_acc_vals <- function(x, 
                           acc_cols = active_acc_cols(x), 
                           non_na = "any") {
  x <- as.data.frame(x) # Drop sticky move2 columns
  non_na <- rlang::arg_match(non_na, values = c("any", "all"))
  
  if (non_na == "any") {
    has_vals <- which(rowSums(!is.na(x[acc_cols])) > 0)
  } else {
    has_vals <- which(rowSums(!is.na(x[acc_cols])) == length(acc_cols))
  }
  
  has_vals
}

group_timestamps <- function(x, tolerance = 0.5) {
  tolerance <- units::as_units(tolerance, "s")
  acc_i <- which_acc_vals(x)
  idx <- split(acc_i, as.character(mt_track_id(x[acc_i, ])))
  
  grps <- lapply(
    idx,
    function(i) {
      d <- units::as_units(diff(mt_time(x[i, ])), "s")
      i[cumsum(c(TRUE, d > tolerance))]
    }
  )
  
  unname(unlist(grps))
}

valid_acc_colsets <- function() {
  list(
    acc_eobs_cols(), 
    acc_burst_cols(),
    acc_raw_xyz_cols(),
    acc_xyz_cols(),
    acc_tilt_cols()
  )
}

is_valid_acc_colset <- function(cols) {
  any(
    unlist(
      lapply(
        valid_acc_colsets(),
        function(x) all(x == cols)
      )
    )
  )
}

has_acc_eobs_cols <- function(x) {
  all(acc_eobs_cols() %in% colnames(x))
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

has_acc_tilt_cols <- function(x) {
  any(acc_tilt_cols() %in% colnames(x))
}

acc_eobs_cols <- function() {
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

acc_tilt_cols <- function() {
  c(
    "tilt_x",
    "tilt_y",
    "tilt_z"
  )
}

active_acc_cols <- function(x) {
  i <- which(
    c(
      has_acc_eobs_cols(x),
      has_acc_burst_cols(x),
      has_acc_raw_xyz_cols(x),
      has_acc_xyz_cols(x),
      has_acc_tilt_cols(x)
    )
  )
  
  if (length(i) == 0) {
    abort_unsupported_cols(x)
  }
  
  colsets <- valid_acc_colsets()[i]
  
  # If multiple column sets present, check if only one has data and use that
  if (length(colsets) > 1) {
    has_vals <- unlist(
      lapply(
        colsets, 
        function(cols) {
          length(which_acc_vals(x, intersect(cols, colnames(x)))) > 0
        }
      )
    )
    
    if (length(which(has_vals)) == 0) {
      # Trivial case, no acc columns have data. Return first set
      colsets <- colsets[1]
    } else if (length(which(has_vals)) > 1) {
      # If multiple have values, use the first one that has values
      colsets <- colsets[which(has_vals)[1]]
      
      rlang::warn(
        c(
          "Detected multiple valid acceleration columns.",
          "i" = paste0(
            "Using `", 
            paste0(colsets[[1]], collapse = "`, `"), "`"
          )
        )
      )
    } else {
      colsets <- colsets[has_vals]
    }
  }
  
  # Intersect to ensure that full set is not returned if only a partial set
  # is present in the data (e.g. `acceleration_x` without `y` or `z`)
  intersect(colsets[[1]], colnames(x))
}

abort_unsupported_cols <- function(x, call = rlang::caller_env()) {
  rlang::abort(
    c(
      "Could not identify a full acceleration column set in the input data.",
      "i" = "Use `valid_acc_colsets()` to see supported acceleration column sets"
    ),
    call = call
  )
}

assert_matched_acc_units <- function(x, cols) {
  for (i in seq_len(length(cols) - 1)) {
    assertthat::assert_that(
      get_units(x[[cols[[1]]]]) == get_units(x[[cols[[i + 1]]]])
    )
  }
}

# Hacky unit comparison for now. Some acc cols don't come with units
get_units <- function(x) {
  if (inherits(x, "units")) {
    units(x)
  } else {
    "None"
  }
}


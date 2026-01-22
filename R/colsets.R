#' Get acceleration columns from a `move2` object
#' 
#' @description
#' Get the names of the acceleration columns that will be used when parsing
#' acceleration bursts in a `move2` object using `as_acc()`. 
#' 
#' In the event
#' that multiple acceleration column sets are present in the input object,
#' the set that contains data values will be identified as the active
#' column set. If multiple sets contain data values, the first such set will
#' be used.
#'
#' @inheritParams as_acc
#' @param quiet Logical indicating whether to warn if multiple acceleration
#'   column sets are detected in `x`.
#'
#' @returns Character vector of acceleration column names
#' @export
active_acc_cols <- function(x, quiet = FALSE) {
  i <- which(
    c(
      has_acc_eobs_cols(x),
      has_acc_burst_cols(x),
      has_acc_xyz_cols(x),
      has_acc_raw_xyz_cols(x),
      has_acc_tilt_cols(x)
    )
  )
  
  if (length(i) == 0) {
    abort_unsupported_cols()
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
      
      if (!quiet) {
        rlang::warn(
          c(
            "Detected multiple valid acceleration column sets.",
            "i" = paste0(
              "Using `", 
              paste0(colsets[[1]], collapse = "`, `"), "`"
            )
          )
        )
      }
    } else {
      colsets <- colsets[has_vals]
    }
  }
  
  # Intersect to ensure that full set is not returned if only a partial set
  # is present in the data (e.g. `acceleration_x` without `y` or `z`)
  intersect(colsets[[1]], colnames(x))
}

#' List valid acceleration data column sets
#'
#' @description
#' These columns are used by `as_acc()` when parsing acceleration bursts
#' contained in a `move2` object. A `move2` object must contain one
#' of these column sets to be processed by `as_acc()`. 
#' 
#' If the `move2` object contains one of the first two sets, it must contain
#' all three of the listed columns in that set. If it contains one of the 
#' final three sets, any subset of the set's listed columns are sufficient.
#'
#' To determine which columns will be used by `as_acc()` for a given
#' `move2` object, see [active_acc_cols()].
#'
#' @returns List of vectors of valid column sets
#' 
#' @export
#' 
#' @examples
#' valid_acc_colsets()
valid_acc_colsets <- function() {
  list(
    acc_eobs_cols(), 
    acc_burst_cols(),
    acc_xyz_cols(),
    acc_raw_xyz_cols(),
    acc_tilt_cols()
  )
}

is_valid_acc_colset <- function(cols) {
  is_acc_eobs_cols(cols) ||
    is_acc_burst_cols(cols) ||
    is_acc_xyz_cols(cols) ||
    is_acc_raw_xyz_cols(cols) ||
    is_acc_tilt_cols(cols)
}

# is_* functions designed to check whether a vector represents a given colset
# while accounting for fact that subsets are allowed for certain colsets
is_acc_eobs_cols <- function(x) {
  setequal(x, acc_eobs_cols()) && length(x) == length(acc_eobs_cols())
}

is_acc_burst_cols <- function(x) {
  setequal(x, acc_burst_cols()) && length(x) == length(acc_burst_cols())
}

is_acc_raw_xyz_cols <- function(x) {
  all(x %in% acc_raw_xyz_cols()) && !anyDuplicated(x)
}

is_acc_xyz_cols <- function(x) {
  all(x %in% acc_xyz_cols()) && !anyDuplicated(x)
}

is_acc_tilt_cols <- function(x) {
  all(x %in% acc_tilt_cols()) && !anyDuplicated(x)
}

# has_* functions designed to check whether an input move2 contains a given
# colset while accounting for fact that subsets are allowed for certain colsets
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

acc_xyz_cols <- function() {
  c(
    "acceleration_x", 
    "acceleration_y", 
    "acceleration_z"
  )
}

acc_raw_xyz_cols <- function() {
  c(
    "acceleration_raw_x", 
    "acceleration_raw_y", 
    "acceleration_raw_z"
  )
}

acc_tilt_cols <- function() {
  c(
    "tilt_x",
    "tilt_y",
    "tilt_z"
  )
}

acc_types <- function() {
  c("eobs", "burst", "xyz", "raw_xyz", "tilt")
}

acc_cols_to_type <- function(acc_cols) {
  i <- purrr::map_lgl(valid_acc_colsets(), function(x) all(acc_cols %in% x))
  acc_types()[i]
}

abort_unsupported_cols <- function(call = rlang::caller_env()) {
  rlang::abort(
    c(
      "Could not identify a full acceleration column set in the input data.",
      "i" = "Use `valid_acc_colsets()` to see supported acceleration column sets."
    ),
    call = call
  )
}

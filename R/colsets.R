#' Specify IMU data columns
#'
#' @description
#' Define which columns in a `move2` object contain IMU data. Pass the
#' result as the `colset` argument of [as_acc()], [as_mag()], or [as_gyro()]
#' to convert those columns into an IMU vector.
#'
#' `imu_colset()` supports two storage formats:
#'
#' - **Long-format**: one measurement per row, one column per axis. Specify
#'   any of `x`, `y`, `z`.
#' - **Burst-format**: each row holds an entire burst as a space-delimited
#'   string. Specify all of `bursts`, `axes`, and `frequency`.
#'
#' The two formats are mutually exclusive within a single colset.
#'
#' @param x,y,z (Long-format) Column name(s) for the X, Y, and/or Z axes.
#' @param bursts (Burst-format) Column name containing the raw burst strings.
#' @param axes (Burst-format) Column name containing the axis labels for
#'   each burst.
#' @param frequency (Burst-format) Column name containing the sampling
#'   frequency for each burst.
#'
#' @returns An `imu_colset` object of type `"long"` or `"burst"`.
#'
#' @seealso [as_acc()], [as_mag()], [as_gyro()] to apply a colset.
#'
#' @export
#'
#' @examples
#' # Long-format: one or more axes
#' imu_colset(x = "my_x", y = "my_y", z = "my_z")
#' imu_colset(x = "my_x", y = "my_y")
#'
#' # Burst-format: all three columns required
#' imu_colset(bursts = "my_raw", axes = "my_axes", frequency = "my_freq")
imu_colset <- function(x = NULL,
                       y = NULL,
                       z = NULL,
                       bursts = NULL,
                       axes = NULL,
                       frequency = NULL) {
  long_args <- purrr::compact(list(X = x, Y = y, Z = z))
  burst_args <- purrr::compact(list(bursts = bursts, axes = axes, frequency = frequency))
  
  has_long <- length(long_args) > 0
  has_burst <- length(burst_args) > 0
  
  if (has_long && has_burst) {
    rlang::abort(c(
      "Cannot mix long-format and burst-format columns in a single imu_colset.",
      i = "Use either `x`/`y`/`z` (long-format) or `bursts`/`axes`/`frequency` (burst-format)."
    ))
  }
  
  if (!has_long && !has_burst) {
    rlang::abort("No IMU data columns specified.")
  }
  
  if (has_burst) {
    if (length(burst_args) != 3) {
      rlang::abort(
        "Burst format requires `bursts`, `axes`, and `frequency` columns."
      )
    }
    
    cols <- unlist(burst_args)
    type <- "burst"
  } else {
    cols <- unlist(long_args)
    type <- "long"
  }
  
  new_imu_colset(cols = cols, type = type)
}

#' @export
print.imu_colset <- function(x, ...) {
  type <- attr(x, "type")
  
  cat(paste0(
    type, "-format [",
    paste0(names(x), "=", unclass(x), collapse = ", "),
    "]\n"
  ))
  
  invisible(x)
}

# Default supported colsets ----------------------------------------------------

#' Valid acceleration data column sets
#'
#' @description
#' Returns the set of acceleration column layouts that `as_acc()` recognizes
#' in a `move2` object. A `move2` object must contain one of these layouts
#' to be processed by [as_acc()].
#'
#' The returned list is named by layout:
#'
#' - `eobs`, `burst` (burst-format): all columns in the set must be present.
#' - `xyz`, `raw_xyz` (long-format): any subset of the axis columns may be
#'   present.
#'
#' To inspect which layouts are active in a given `move2` object, see
#' [active_acc_colsets()]. To build a custom layout, use [imu_colset()].
#'
#' @returns A named list of `imu_colset` objects.
#'
#' @export
#'
#' @examples
#' valid_acc_colsets()
valid_acc_colsets <- function() {
  purrr::map(acc_colset_config(), function(colset) colset$cols)
}

acc_colset_eobs <- function() {
  new_imu_colset(
    cols = c(
      axes = "eobs_acceleration_axes",
      frequency = "eobs_acceleration_sampling_frequency_per_axis",
      bursts = "eobs_accelerations_raw"
    ),
    type = "burst"
  )
}

acc_colset_burst <- function() {
  new_imu_colset(
    cols = c(
      axes = "acceleration_axes",
      frequency = "acceleration_sampling_frequency_per_axis",
      bursts = "accelerations_raw"
    ),
    type = "burst"
  )
}

acc_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "acceleration_x",
      Y = "acceleration_y",
      Z = "acceleration_z"
    ),
    type = "long"
  )
}

acc_colset_raw_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "acceleration_raw_x",
      Y = "acceleration_raw_y",
      Z = "acceleration_raw_z"
    ),
    type = "long"
  )
}

#' Valid magnetometer data column sets
#'
#' @description
#' Returns the set of magnetometer column layouts that `as_mag()` recognizes
#' in a `move2` object. A `move2` object must contain one of these layouts
#' to be processed by [as_mag()].
#'
#' The returned list is named by layout:
#'
#' - `burst` (burst-format): all columns in the set must be present.
#' - `xyz`, `raw_xyz` (long-format): any subset of the axis columns may be
#'   present.
#'
#' To inspect which layouts are active in a given `move2` object, see
#' [active_mag_colsets()]. To build a custom layout, use [imu_colset()].
#'
#' @returns A named list of `imu_colset` objects.
#'
#' @export
#'
#' @examples
#' valid_mag_colsets()
valid_mag_colsets <- function() {
  purrr::map(mag_colset_config(), function(colset) colset$cols)
}

mag_colset_burst <- function() {
  new_imu_colset(
    cols = c(
      bursts = "magnetic_fields_raw",
      axes = "magnetic_field_axes",
      frequency = "magnetic_field_sampling_frequency_per_axis"
    ),
    type = "burst"
  )
}

mag_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "magnetic_field_x",
      Y = "magnetic_field_y",
      Z = "magnetic_field_z"
    ),
    type = "long"
  )
}

mag_colset_raw_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "magnetic_field_raw_x",
      Y = "magnetic_field_raw_y",
      Z = "magnetic_field_raw_z"
    ),
    type = "long"
  )
}

#' Valid gyroscope data column sets
#'
#' @description
#' Returns the set of gyroscope column layouts that `as_gyro()` recognizes
#' in a `move2` object. A `move2` object must contain one of these layouts
#' to be processed by [as_gyro()].
#'
#' The returned list is named by layout:
#'
#' - `burst` (burst-format): all columns in the set must be present.
#' - `xyz` (long-format): any subset of the axis columns may be present.
#'
#' To inspect which layouts are active in a given `move2` object, see
#' [active_gyro_colsets()]. To build a custom layout, use [imu_colset()].
#'
#' @returns A named list of `imu_colset` objects.
#'
#' @export
#'
#' @examples
#' valid_gyro_colsets()
valid_gyro_colsets <- function() {
  purrr::map(gyro_colset_config(), function(colset) colset$cols)
}

gyro_colset_burst <- function() {
  new_imu_colset(
    cols = c(
      bursts = "angular_velocities_raw",
      axes = "gyroscope_axes",
      frequency = "gyroscope_sampling_frequency_per_axis"
    ),
    type = "burst"
  )
}

gyro_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "angular_velocity_x",
      Y = "angular_velocity_y",
      Z = "angular_velocity_z"
    ),
    type = "long"
  )
}

# Colsets in a move2 object ----------------------------------------------------

#' Identify acceleration columns present in a `move2` object
#'
#' @description
#' `active_acc_colsets()` determines the sets of columns that will be used
#' by default to construct acceleration bursts with `as_acc()`. Column sets
#' are processed independently, but a given `move2` may contain multiple
#' column sets with acceleration data.
#'
#' @details
#' `move2` objects store acceleration data in two ways: long-format
#' acceleration columns and burst-format acceleration columns.
#'
#' Long-format columns store one acceleration measurement (possibly for multiple
#' axes) in a single row. Note that
#' not all axes need to be present for a long-format column set to be considered
#' active in a `move2` object.
#'
#' Burst-format columns store a burst of acceleration data as a space-delimited
#' string. This string must be segmented into axis-specific measurements using
#' an associated column that indicates the axes present for the bursted data.
#' A further column provides the sampling frequency of the burst.
#' All three of these columns must be present for a burst-format column set
#' to be considered active in a `move2` object.
#'
#' Standard long-format column sets for data from
#' Movebank can be found with [valid_acc_colsets()].
#'
#' If your input data use different column names for these columns, use
#' [imu_colset()] to specify
#' the column names that correspond to each of the axes (for long-format
#' data) or burst data and associated metadata (for burst-format data).
#'
#' @inheritParams as_acc
#'
#' @returns A list of `imu_colset` objects.
#'
#' @export
#' @rdname acc-cols
#'
#' @seealso [valid_acc_colsets()] for currently supported default colsets,
#'   [as_acc()] to build an `acc` vector from a `move2` object.
#'
#' @examples
#' active_acc_colsets(albatrosses())
#'
#' # Multiple colsets may be available
#' active_acc_colsets(move2::mt_stack(albatrosses(), gulls()))
#'
#' # Missing long-format axes are not included in the set
#' g <- gulls()
#' g$acceleration_raw_x <- NULL
#' active_acc_colsets(g)
#'
#' # Columns with no data are also removed
#' g$acceleration_raw_y <- NA
#' active_acc_colsets(g)
#'
#' # Some column sets must be present in their entirety
#' alb <- albatrosses()
#' alb$eobs_acceleration_axes <- NULL
#'
#' \dontrun{
#'   active_acc_colsets(alb)
#' }
active_acc_colsets <- function(x) {
  active_colsets_(x, "acc")
}

#' Identify magnetometer columns present in a `move2` object
#'
#' @description
#' `active_mag_colsets()` determines the sets of columns that will be used
#' by default to construct magnetometer bursts with `as_mag()`. Column sets
#' are processed independently, but a given `move2` may contain multiple
#' column sets with magnetometer data.
#'
#' @details
#' `move2` objects store magnetometer data in two ways: long-format
#' magnetometer columns and burst-format magnetometer columns.
#'
#' Long-format columns store one magnetometer measurement (possibly for multiple
#' axes) in a single row. Note that
#' not all axes need to be present for a long-format column set to be considered
#' active in a `move2` object.
#'
#' Burst-format columns store a burst of magnetometer data as a space-delimited
#' string. This string must be segmented into axis-specific measurements using
#' an associated column that indicates the axes present for the bursted data.
#' A further column provides the sampling frequency of the burst.
#' All three of these columns must be present for a burst-format column set
#' to be considered active in a `move2` object.
#'
#' Standard long-format column sets for data from
#' Movebank can be found with [valid_mag_colsets()].
#'
#' If your input data use different column names for these columns, use
#' [imu_colset()] to specify
#' the column names that correspond to each of the axes (for long-format
#' data) or burst data and associated metadata (for burst-format data).
#'
#' @inheritParams as_mag
#'
#' @returns A list of `imu_colset` objects.
#'
#' @export
#'
#' @seealso [valid_mag_colsets()] for currently supported default colsets,
#'   [as_mag()] to build a `mag` vector from a `move2` object.
active_mag_colsets <- function(x) {
  active_colsets_(x, "mag")
}

#' Identify gyroscope columns present in a `move2` object
#'
#' @description
#' `active_gyro_colsets()` determines the sets of columns that will be used
#' by default to construct gyroscope bursts with `as_gyro()`. Column sets
#' are processed independently, but a given `move2` may contain multiple
#' column sets with gyroscope data.
#'
#' @details
#' `move2` objects store gyroscope data in two ways: long-format
#' gyroscope columns and burst-format gyroscope columns.
#'
#' Long-format columns store one gyroscope measurement (possibly for multiple
#' axes) in a single row. Note that
#' not all axes need to be present for a long-format column set to be considered
#' active in a `move2` object.
#'
#' Burst-format columns store a burst of gyroscope data as a space-delimited
#' string. This string must be segmented into axis-specific measurements using
#' an associated column that indicates the axes present for the bursted data.
#' A further column provides the sampling frequency of the burst.
#' All three of these columns must be present for a burst-format column set
#' to be considered active in a `move2` object.
#'
#' The standard long-format column set for data from Movebank can be found
#' with [valid_gyro_colsets()].
#'
#' If your input data use different column names for these columns, use
#' [imu_colset()] to specify
#' the column names that correspond to each of the axes (for long-format
#' data) or burst data and associated metadata (for burst-format data).
#'
#' @inheritParams as_gyro
#'
#' @returns A list of `imu_colset` objects.
#'
#' @export
#'
#' @seealso [valid_gyro_colsets()] for currently supported default colsets,
#'   [as_gyro()] to build a `gyro` vector from a `move2` object.
active_gyro_colsets <- function(x) {
  active_colsets_(x, "gyro")
}

# Apply active colset logic in a move2 for a given IMU class. Active colsets
# are fully present (if burst-format) and contain data.
active_colsets_ <- function(x, sensor) {
  config <- switch(
    sensor,
    acc = acc_colset_config(),
    mag = mag_colset_config(),
    gyro = gyro_colset_config()
  )
  i <- which(purrr::map_lgl(config, function(colset) colset$is_in_(x)))
  
  if (length(i) == 0) {
    abort_missing_colset(sensor)
  }
  
  poss_colsets <- config[i]
  
  colsets <- purrr::compact(
    purrr::map(
      poss_colsets,
      function(colset_config) {
        colset <- colset_config$cols
        cols_in_x <- intersect(colset, colnames(x))
        cols_present <- cols_in_x[!cols_empty(x, cols_in_x)]
        
        if (!identical(cols_present, as.character(colset))) {
          if (attr(colset, "type") == "burst") {
            # Remove entire colset for types that require all cols present
            return(NULL)
          }
          
          # Rebuild long-format colset with only present columns
          return(new_imu_colset(cols = cols_present, type = attr(colset, "type")))
        }
        
        colset
      }
    )
  )
  
  if (length(colsets) == 0) {
    abort_missing_colset(sensor)
  }
  
  colsets
}

#' Identify rows with acceleration data from multiple column sets
#'
#' This function returns the row indices of a `move2` object
#' where more than one acceleration column set contains data. `as_acc()`
#' refuses to build `acc` objects for rows where multiple input sources exist.
#' These rows can be modified to remove data from additional column sets.
#' Alternatively, specific columns can be passed to the `colset` argument
#' of `as_acc()` to avoid processing duplicated records.
#'
#' @inheritParams as_acc
#' @param colsets List of `imu_colset` objects to check for
#'   overlap. Defaults to the column sets detected by [active_acc_colsets()].
#'
#' @returns An integer vector of row indices with duplicated acceleration data
#'   across column sets.
#'
#' @seealso
#'   - [active_acc_colsets()] to identify available column sets in a `move2` object.
#'   - [as_acc()] to generate an `acc` vector from a `move2` object.
#'
#' @export
duplicated_acc_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_acc_colsets(x))
}

#' Identify rows with magnetometer data from multiple column sets
#'
#' This function returns the row indices of a `move2` object
#' where more than one magnetometer column set contains data. `as_mag()`
#' refuses to build `mag` objects for rows where multiple input sources exist.
#' These rows can be modified to remove data from additional column sets.
#' Alternatively, specific columns can be passed to the `colset` argument
#' of `as_mag()` to avoid processing duplicated records.
#'
#' @inheritParams as_mag
#' @param colsets List of `imu_colset` objects to check for
#'   overlap. Defaults to the column sets detected by [active_mag_colsets()].
#'
#' @returns An integer vector of row indices with duplicated magnetometer data
#'   across column sets.
#'
#' @seealso
#'   - [active_mag_colsets()] to identify available column sets in a `move2` object.
#'   - [as_mag()] to generate a `mag` vector from a `move2` object.
#'
#' @export
duplicated_mag_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_mag_colsets(x))
}

#' Identify rows with gyroscope data from multiple column sets
#'
#' This function returns the row indices of a `move2` object
#' where more than one gyroscope column set contains data. `as_gyro()`
#' refuses to build `gyro` objects for rows where multiple input sources exist.
#' These rows can be modified to remove data from additional column sets.
#' Alternatively, specific columns can be passed to the `colset` argument
#' of `as_gyro()` to avoid processing duplicated records.
#'
#' @inheritParams as_gyro
#' @param colsets List of `imu_colset` objects to check for
#'   overlap. Defaults to the column sets detected by [active_gyro_colsets()].
#'
#' @returns An integer vector of row indices with duplicated gyroscope data
#'   across column sets.
#'
#' @seealso
#'   - [active_gyro_colsets()] to identify available column sets in a `move2` object.
#'   - [as_gyro()] to generate a `gyro` vector from a `move2` object.
#'
#' @export
duplicated_gyro_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_gyro_colsets(x))
}

duplicated_imu_rows <- function(x, colsets = NULL) {
  # Standardize case where user supplied a single colset as a vector
  if (!rlang::is_list(colsets)) {
    colsets <- list(colsets)
  }
  
  rows <- unlist(
    purrr::map(
      colsets,
      function(cols) which_imu_vals(x, colset = cols)
    )
  )
  
  # Would be nice to return duplicated groups too so user knows what the issue is...
  sort(unique(rows[duplicated(rows) | duplicated(rows, fromLast = TRUE)]))
}

# Colset construction helpers --------------------------------------------------

# Internal constructor for `imu_colset` objects. Colsets are IMU-class-agnostic:
# the same colset can be passed to `as_acc()`, `as_mag()`, or `as_gyro()` -
# the IMU class is determined by which converter you call, not by the colset.
new_imu_colset <- function(cols, type) {
  type <- rlang::arg_match(type, c("long", "burst"))
  
  structure(
    cols,
    type = type,
    class = c("imu_colset", class(cols))
  )
}

is_imu_colset <- function(x) {
  inherits(x, "imu_colset")
}

# Colset config and validation -------------------------------------------------

# Registry of supported default colsets. Each entry is built
# via `register_colset()`, which derives the appropriate is_/is_in_ checks from
# the colset's own `type` attribute.
acc_colset_config <- function() {
  list(
    eobs = register_colset(acc_colset_eobs()),
    burst = register_colset(acc_colset_burst()),
    xyz = register_colset(acc_colset_xyz()),
    raw_xyz = register_colset(acc_colset_raw_xyz())
  )
}

mag_colset_config <- function() {
  list(
    burst = register_colset(mag_colset_burst()),
    xyz = register_colset(mag_colset_xyz()),
    raw_xyz = register_colset(mag_colset_raw_xyz())
  )
}

gyro_colset_config <- function() {
  list(
    burst = register_colset(gyro_colset_burst()),
    xyz = register_colset(gyro_colset_xyz())
  )
}

# Build a single config entry from a colset.
#
# - "burst" type colsets require all columns to be present
# - "long" type colsets allow subsets of the axis cols
#
# - `is_` checks whether a colset vector matches this entry (including subsets
#   for long format)
# - `is_in_` checks whether a `move2` object contains the required columns
#   (including subsets for long format)
register_colset <- function(cols) {
  if (attr(cols, "type") == "burst") {
    list(
      cols = cols,
      is_ = function(x) setequal(x, cols) && length(x) == length(cols),
      is_in_ = function(m) all(cols %in% colnames(m))
    )
  } else {
    list(
      cols = cols,
      is_ = function(x) is_unique_named_subset(x, cols),
      is_in_ = function(m) any(cols %in% colnames(m))
    )
  }
}

# General helpers --------------------------------------------------------------

# Check that `x` is a non-empty, non-duplicated, name-value subset of `target`
is_unique_named_subset <- function(x, y) {
  length(x) > 0 &&
    anyDuplicated(names(x)) == 0 &&
    identical(x[names(x)], y[names(x)])
}

cols_empty <- function(x, cols) {
  assert_all_cols_present(x, cols)
  purrr::map_lgl(
    cols,
    function(col) all(is.na(unlist(x[[col]]))) || all(rlang::is_empty(unlist(x[[col]])))
  )
}

assert_all_cols_present <- function(x, cols, call = rlang::caller_env()) {
  if (!all(cols %in% colnames(x))) {
    cols <- cols[which(!cols %in% colnames(x))]
    
    rlang::abort(
      c(
        "Missing columns provided.",
        x = paste0("Could not find columns \"", paste(cols, collapse = "\", \""), "\"")
      ),
      call = call
    )
  }
}

abort_missing_colset <- function(sensor, call = rlang::caller_env()) {
  rlang::abort(
    c(
      paste0("Could not identify a full ", sensor, " column set in the input data."),
      "i" = paste0(
        "Use `valid_", sensor, "_colsets()` to see supported ", sensor, " column sets."
      )
    ),
    call = call
  )
}

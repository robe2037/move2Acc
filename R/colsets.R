#' Specify acceleration data columns
#'
#' @description
#' Define which columns in a `move2` object contain acceleration data.
#' Use this function to manually specify the columns that should be used when
#' constructing an `acc` vector with `as_acc()`.
#'
#' For long-format data (one measurement per row), specify axis columns
#' with `x`, `y`, and/or `z`.
#'
#' For burst-format data (where each row contains burst data in a single string),
#' specify the columns containing the raw burst data, axes, and sampling
#' frequency.
#'
#' @param x,y,z In long-format data, the column name(s) for the
#'   X, Y, and/or Z acceleration axes.
#' @param bursts For burst-format data, the column name containing the raw burst
#'   strings.
#' @param axes For burst-format data, the column name containing the axis
#'   labels.
#' @param frequency For burst-format data, the column name containing the
#'   sampling frequency.
#'
#' @returns An `acc_colset` object.
#'
#' @seealso [active_acc_colsets()] for automatic colset detection, [as_acc()] to
#'   construct an `acc` vector using a colset specification.
#'
#' @export
#'
#' @examples
#' # Specify the column names for long-format data for each axis
#' acc_colset(x = "my_acc_x", y = "my_acc_y", z = "my_acc_z")
#'
#' # Long format data may consist of a subset of axes
#' acc_colset(x = "my_acc_x", y = "my_acc_y")
#'
#' # Specify the column names for the bursts, axes, and frequency for
#' # burst-format data
#' acc_colset(
#'   bursts = "my_raw_acc",
#'   axes = "my_axes",
#'   frequency = "my_freq"
#' )
acc_colset <- function(x = NULL,
                       y = NULL,
                       z = NULL,
                       bursts = NULL,
                       axes = NULL,
                       frequency = NULL) {
  build_colset_(
    sensor = "acc",
    x = x,
    y = y,
    z = z,
    bursts = bursts,
    axes = axes,
    frequency = frequency
  )
}

#' Specify magnetometer data columns
#'
#' @description
#' Define which columns in a `move2` object contain magnetometer data.
#' Use this function to manually specify the columns that should be used when
#' constructing a `mag` vector with `as_mag()`.
#'
#' For long-format data (one measurement per row), specify axis columns
#' with `x`, `y`, and/or `z`.
#'
#' For burst-format data (where each row contains burst data in a single string),
#' specify the columns containing the raw burst data, axes, and sampling
#' frequency.
#'
#' @param x,y,z In long-format data, the column name(s) for the
#'   X, Y, and/or Z magnetometer axes.
#' @param bursts For burst-format data, the column name containing the raw burst
#'   strings.
#' @param axes For burst-format data, the column name containing the axis
#'   labels.
#' @param frequency For burst-format data, the column name containing the
#'   sampling frequency.
#'
#' @returns A `mag_colset` object.
#'
#' @seealso [active_mag_colsets()] for automatic colset detection, [as_mag()] to
#'   construct a `mag` vector using a colset specification.
#'
#' @export
#'
#' @examples
#' # Specify the column names for long-format data for each axis
#' mag_colset(x = "my_mag_x", y = "my_mag_y", z = "my_mag_z")
#'
#' # Long format data may consist of a subset of axes
#' mag_colset(x = "my_mag_x", y = "my_mag_y")
#'
#' # Specify the column names for the bursts, axes, and frequency for
#' # burst-format data
#' mag_colset(
#'   bursts = "my_raw_mag",
#'   axes = "my_axes",
#'   frequency = "my_freq"
#' )
mag_colset <- function(x = NULL,
                       y = NULL,
                       z = NULL,
                       bursts = NULL,
                       axes = NULL,
                       frequency = NULL) {
  build_colset_(
    sensor = "mag",
    x = x,
    y = y,
    z = z,
    bursts = bursts,
    axes = axes,
    frequency = frequency
  )
}

#' @export
print.colset <- function(x, ...) {
  type <- attr(x, "type")
  sensor <- class(x)[1]
  
  if (type == "long") {
    cat(paste0(
      "<", sensor, "> long-format [",
      paste0(names(x), "=", unclass(x), collapse = ", "),
      "]\n"
    ))
  } else {
    cat(paste0(
      "<", sensor, "> burst-format [",
      paste0(names(x), "=", unclass(x), collapse = ", "),
      "]\n"
    ))
  }
  invisible(x)
}

# Default supported colsets --------------------------------------==------------

#' Valid acceleration data column sets
#'
#' @description
#' These sets of columns can be used by [as_acc()] when parsing acceleration
#' bursts contained in a `move2` object. A `move2` object must contain one
#' of these column sets to be processed by `as_acc()`.
#'
#' - `acc_colset_eobs()` and `acc_colset_burst()` must be present in their entirety
#' within a data source to be used when parsing acceleration data.
#' - For `acc_colset_xyz()` and `acc_colset_raw_xyz()`, any
#' subset of the set's columns can be used to parse acceleration
#' data.
#'
#' To determine the default columns that will be used by `as_acc()` for a given
#' `move2` object, see [active_acc_colsets()].
#'
#' @returns For `valid_acc_colsets()`, a list of `acc_colset` objects
#'   containing valid column sets. Otherwise, an `acc_colset` object.
#'
#' @export
#'
#' @examples
#' valid_acc_colsets()
valid_acc_colsets <- function() {
  purrr::map(acc_colset_config(), function(colset) colset$cols)
}

#' @export
#' @rdname valid_acc_colsets
acc_colset_eobs <- function() {
  new_colset(
    cols = c(
      axes = "eobs_acceleration_axes",
      frequency = "eobs_acceleration_sampling_frequency_per_axis",
      bursts = "eobs_accelerations_raw"
    ),
    type = "burst",
    sensor = "acc"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_colset_burst <- function() {
  new_colset(
    cols = c(
      axes = "acceleration_axes",
      frequency = "acceleration_sampling_frequency_per_axis",
      bursts = "accelerations_raw"
    ),
    type = "burst",
    sensor = "acc"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_colset_xyz <- function() {
  new_colset(
    cols = c(
      X = "acceleration_x",
      Y = "acceleration_y",
      Z = "acceleration_z"
    ),
    type = "long",
    sensor = "acc"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_colset_raw_xyz <- function() {
  new_colset(
    cols = c(
      X = "acceleration_raw_x",
      Y = "acceleration_raw_y",
      Z = "acceleration_raw_z"
    ),
    type = "long",
    sensor = "acc"
  )
}

#' Valid magnetometer data column sets
#'
#' @description
#' These sets of columns can be used by [as_mag()] when parsing magnetometer
#' bursts contained in a `move2` object. A `move2` object must contain one
#' of these column sets to be processed by `as_mag()`.
#'
#' - `mag_colset_burst()` must be present in its entirety within a data
#' source to be used when parsing magnetometer data.
#' - For `mag_colset_xyz()` and `mag_colset_raw_xyz()`, any subset of the
#' set's columns can be used to parse magnetometer data.
#'
#' To determine the default columns that will be used by `as_mag()` for a given
#' `move2` object, see [active_mag_colsets()].
#'
#' @returns For `valid_mag_colsets()`, a list of `mag_colset` objects
#'   containing valid column sets. Otherwise, a `mag_colset` object.
#'
#' @export
#'
#' @examples
#' valid_mag_colsets()
valid_mag_colsets <- function() {
  purrr::map(mag_colset_config(), function(colset) colset$cols)
}

#' @export
#' @rdname valid_mag_colsets
mag_colset_burst <- function() {
  new_colset(
    cols = c(
      bursts = "magnetic_fields_raw",
      axes = "magnetic_field_axes",
      frequency = "magnetic_field_sampling_frequency_per_axis"
    ),
    type = "burst",
    sensor = "mag"
  )
}

#' @export
#' @rdname valid_mag_colsets
mag_colset_xyz <- function() {
  new_colset(
    cols = c(
      X = "magnetic_field_x",
      Y = "magnetic_field_y",
      Z = "magnetic_field_z"
    ),
    type = "long",
    sensor = "mag"
  )
}

#' @export
#' @rdname valid_mag_colsets
mag_colset_raw_xyz <- function() {
  new_colset(
    cols = c(
      X = "magnetic_field_raw_x",
      Y = "magnetic_field_raw_y",
      Z = "magnetic_field_raw_z"
    ),
    type = "long",
    sensor = "mag"
  )
}

#' Specify gyroscope data columns
#'
#' @description
#' Define which columns in a `move2` object contain gyroscope data.
#' Use this function to manually specify the columns that should be used when
#' constructing a `gyro` vector with `as_gyro()`.
#'
#' For long-format data (one measurement per row), specify axis columns
#' with `x`, `y`, and/or `z`.
#'
#' For burst-format data (where each row contains burst data in a single string),
#' specify the columns containing the raw burst data, axes, and sampling
#' frequency.
#'
#' @param x,y,z In long-format data, the column name(s) for the
#'   X, Y, and/or Z gyroscope axes.
#' @param bursts For burst-format data, the column name containing the raw burst
#'   strings.
#' @param axes For burst-format data, the column name containing the axis
#'   labels.
#' @param frequency For burst-format data, the column name containing the
#'   sampling frequency.
#'
#' @returns A `gyro_colset` object.
#'
#' @seealso [active_gyro_colsets()] for automatic colset detection, [as_gyro()]
#'   to construct a `gyro` vector using a colset specification.
#'
#' @export
#'
#' @examples
#' # Specify the column names for long-format data for each axis
#' gyro_colset(x = "my_gyro_x", y = "my_gyro_y", z = "my_gyro_z")
#'
#' # Long format data may consist of a subset of axes
#' gyro_colset(x = "my_gyro_x", y = "my_gyro_y")
#'
#' # Specify the column names for the bursts, axes, and frequency for
#' # burst-format data
#' gyro_colset(
#'   bursts = "my_raw_gyro",
#'   axes = "my_axes",
#'   frequency = "my_freq"
#' )
gyro_colset <- function(x = NULL,
                        y = NULL,
                        z = NULL,
                        bursts = NULL,
                        axes = NULL,
                        frequency = NULL) {
  build_colset_(
    sensor = "gyro",
    x = x,
    y = y,
    z = z,
    bursts = bursts,
    axes = axes,
    frequency = frequency
  )
}

#' Valid gyroscope data column sets
#'
#' @description
#' These sets of columns can be used by [as_gyro()] when parsing gyroscope
#' bursts contained in a `move2` object. A `move2` object must contain one
#' of these column sets to be processed by `as_gyro()`.
#'
#' - `gyro_colset_burst()` must be present in its entirety within a data
#' source to be used when parsing gyroscope data.
#' - For `gyro_colset_xyz()`, any subset of the set's columns can be used to
#' parse gyroscope data.
#'
#' To determine the default columns that will be used by `as_gyro()` for a given
#' `move2` object, see [active_gyro_colsets()].
#'
#' @returns For `valid_gyro_colsets()`, a list of `gyro_colset` objects
#'   containing valid column sets. Otherwise, a `gyro_colset` object.
#'
#' @export
#'
#' @examples
#' valid_gyro_colsets()
valid_gyro_colsets <- function() {
  purrr::map(gyro_colset_config(), function(colset) colset$cols)
}

#' @export
#' @rdname valid_gyro_colsets
gyro_colset_burst <- function() {
  new_colset(
    cols = c(
      bursts = "angular_velocities_raw",
      axes = "gyroscope_axes",
      frequency = "gyroscope_sampling_frequency_per_axis"
    ),
    type = "burst",
    sensor = "gyro"
  )
}

#' @export
#' @rdname valid_gyro_colsets
gyro_colset_xyz <- function() {
  new_colset(
    cols = c(
      X = "angular_velocity_x",
      Y = "angular_velocity_y",
      Z = "angular_velocity_z"
    ),
    type = "long",
    sensor = "gyro"
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
#' Movebank include [acc_colset_raw_xyz()] and [acc_colset_xyz()]. Standard
#' burst-format column sets for data from Movebank include [acc_colset_eobs()]
#' and [acc_colset_burst()].
#'
#' If your input data use different column names for these columns, use
#' [acc_colset()] to specify the column names that correspond to each of the
#' axes (for long-format data) or burst data and associated metadata (for
#' burst-format data).
#'
#' @inheritParams as_acc
#'
#' @returns A list of `acc_colset` objects.
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
#' Movebank include [mag_colset_raw_xyz()] and [mag_colset_xyz()]. The standard
#' burst-format column set for data from Movebank is [mag_colset_burst()].
#'
#' If your input data use different column names for these columns, use
#' [mag_colset()] to specify the column names that correspond to each of the
#' axes (for long-format data) or burst data and associated metadata (for
#' burst-format data).
#'
#' @inheritParams as_mag
#'
#' @returns A list of `mag_colset` objects.
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
#' The standard long-format column set for data from Movebank is
#' [gyro_colset_xyz()]. The standard burst-format column set for data from
#' Movebank is [gyro_colset_burst()].
#'
#' If your input data use different column names for these columns, use
#' [gyro_colset()] to specify the column names that correspond to each of the
#' axes (for long-format data) or burst data and associated metadata (for
#' burst-format data).
#'
#' @inheritParams as_gyro
#'
#' @returns A list of `gyro_colset` objects.
#'
#' @export
#'
#' @seealso [valid_gyro_colsets()] for currently supported default colsets,
#'   [as_gyro()] to build a `gyro` vector from a `move2` object.
active_gyro_colsets <- function(x) {
  active_colsets_(x, "gyro")
}

# Apply active colset logic in a move2 for a given sensor. Active colsets
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
          return(
            new_colset(
              cols = cols_present,
              type = attr(colset, "type"),
              sensor = sensor
            )
          )
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
#' @param colsets List of `acc_colset` objects to check for
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
  duplicated_sensor_rows(x, colsets %||% active_acc_colsets(x))
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
#' @param colsets List of `mag_colset` objects to check for
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
  duplicated_sensor_rows(x, colsets %||% active_mag_colsets(x))
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
#' @param colsets List of `gyro_colset` objects to check for
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
  duplicated_sensor_rows(x, colsets %||% active_gyro_colsets(x))
}

duplicated_sensor_rows <- function(x, colsets = NULL) {
  # Standardize case where user supplied a single colset as a vector
  if (!rlang::is_list(colsets)) {
    colsets <- list(colsets)
  }
  
  rows <- unlist(
    purrr::map(
      colsets,
      function(cols) which_sensor_vals(x, colset = cols)
    )
  )
  
  # Would be nice to return duplicated groups too so user knows what the issue is...
  sort(unique(rows[duplicated(rows) | duplicated(rows, fromLast = TRUE)]))
}
# Colset construction helpers --------------------------------------------------

# Shared argument handling for sensor colset constructors. Standardizes the
# parsing of long-format vs. burst-format columns across sensor types, as
# several sensors have similar Movebank column conventions
build_colset_ <- function(sensor,
                          x = NULL,
                          y = NULL,
                          z = NULL,
                          bursts = NULL,
                          axes = NULL,
                          frequency = NULL,
                          call = rlang::caller_env()) {
  long_args <- purrr::compact(list(X = x, Y = y, Z = z))
  burst_args <- purrr::compact(list(bursts, axes, frequency))

  has_long <- length(long_args) > 0
  has_burst <- length(burst_args) > 0

  if (has_long && has_burst) {
    rlang::abort("Specify axis columns or burst columns, not both.", call = call)
  }

  if (!has_long && !has_burst) {
    rlang::abort(paste0("No ", sensor, " columns specified."), call = call)
  }

  if (has_burst) {
    if (length(burst_args) != 3) {
      rlang::abort(
        "Burst format requires `bursts`, `axes`, and `frequency` columns.",
        call = call
      )
    }

    cols <- c(bursts = bursts, axes = axes, frequency = frequency)
    type <- "burst"
  } else {
    cols <- unlist(long_args)
    type <- "long"
  }

  new_colset(cols = cols, type = type, sensor = sensor)
}

# Internal constructor for sensor colset objects. Each sensor exposes its own
# public constructor (e.g. `acc_colset()`) which delegates here.
# The `colset` parent class enables shared S3 dispatch for methods that do
# not depend on sensor type (e.g. `print`).
new_colset <- function(cols, type, sensor) {
  type <- rlang::arg_match(type, c("long", "burst"))
  sensor <- rlang::arg_match(sensor, valid_sensors())

  structure(
    cols,
    type = type,
    class = c(paste0(sensor, "_colset"), "colset", class(cols))
  )
}

is_acc_colset <- function(x) {
  inherits(x, "acc_colset")
}

is_mag_colset <- function(x) {
  inherits(x, "mag_colset")
}

is_gyro_colset <- function(x) {
  inherits(x, "gyro_colset")
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

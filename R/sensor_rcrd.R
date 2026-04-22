# Registry of supported sensor subclasses. Extend this when adding a new
# concrete sensor. Helpers like `assert_sensor_rcrd()` and
# `parse_colsets()` read from it.
valid_sensors <- function() {
  c("acc", "mag", "gyro")
}

# Parent class `sensor_rcrd` defines the shared (bursts, frequency, start)
# record shape for various sensors that vary by subclass (e.g. `acc`)
sensor_rcrd <- function(sensor,
                        bursts = list(),
                        frequency = units::set_units(double(), "Hz"),
                        start = NULL) {
  bursts <- burst_list(bursts, sensor)
  n <- vec_size(bursts)

  if (!inherits(frequency, "units")) {
    frequency <- units::set_units(frequency, "Hz")
  } else if (!units::ud_are_convertible(units::deparse_unit(frequency), "Hz")) {
    rlang::abort("`frequency` must be convertible to a frequency unit.")
  }

  start <- start %||% NA_real_

  if (inherits(start, "POSIXt")) {
    tz <- attr(start, "tzone")
  } else {
    tz <- "UTC"
  }

  start <- as.POSIXct(as.double(start), tz = tz)

  frequency <- vec_recycle(frequency, n)
  start <- vec_recycle(start, n)

  # Ensure metadata is NA when bursts are missing, so that the record is
  # consistently all-NA and vec_detect_missing() agrees with is.na()
  na_burst <- vec_detect_missing(bursts)

  if (any(na_burst)) {
    frequency[na_burst] <- units::set_units(NA, "Hz")
    start[na_burst] <- as.POSIXct(NA, tz = tz)
  }

  new_sensor_rcrd(
    sensor,
    bursts = bursts,
    frequency = frequency,
    start = start
  )
}

new_sensor_rcrd <- function(sensor,
                            bursts = new_burst_list(list(), sensor),
                            frequency = units::set_units(double(), "Hz"),
                            start = as.POSIXct(double(), tz = "UTC")) {
  new_rcrd(
    list(bursts = bursts, frequency = frequency, start = start),
    class = c(sensor, "sensor_rcrd")
  )
}

burst_list <- function(x, sensor) {
  valid_axes <- c("X", "Y", "Z")

  is_valid <- purrr::map_lgl(
    x,
    function(b) {
      if (is.null(b)) return(TRUE)
      nms <- colnames(b)
      !is.null(nms) && length(nms) > 0 && all(nms %in% valid_axes)
    }
  )

  if (any(!is_valid)) {
    rlang::abort("Burst matrix columns must be named \"X\", \"Y\", or \"Z\".")
  }

  new_burst_list(x, sensor)
}

new_burst_list <- function(x, sensor) {
  new_list_of(
    x,
    ptype = matrix(numeric()),
    class = c(paste0(sensor, "_list"), "burst_list")
  )
}

# Assert that x is one of the supported sensor vector classes. Centralizes the
# check so the error message lists concrete subclasses (e.g. `acc`, `mag`)
# without exposing the internal `sensor_rcrd` parent class.
assert_sensor_rcrd <- function(x,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (inherits(x, "sensor_rcrd")) return(invisible(x))

  sensors <- valid_sensors()
  names <- paste0("`", sensors, "`")

  msg <- paste0(
    "`", arg, "` must be ",
    if (length(sensors) == 1) {
      paste0("an ", names, " vector.")
    } else if (length(sensors) == 2) {
      paste0("an ", names[1], " or ", names[2], " vector.")
    } else {
      # Oxford-comma list for three or more: "an `a`, `b`, or `c` vector."
      paste0(
        "an ",
        paste0(names[-length(names)], collapse = ", "),
        ", or ", names[length(names)], " vector."
      )
    }
  )

  rlang::abort(msg, call = call)
}

#' @export
is.na.sensor_rcrd <- function(x) {
  vctrs::vec_detect_missing(x)
}

# Shared implementations of vctrs type-combination logic. Subclasses need
# their own methods, but can simply call these methods
sensor_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  freq_common <- vctrs::vec_ptype2(freqs(x), freqs(y))
  start_common <- vctrs::vec_ptype2(starts(x), starts(y))

  new_sensor_rcrd(
    class(x)[1],
    frequency = freq_common,
    start = start_common
  )
}

sensor_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  freqs(x) <- units::set_units(freqs(x), units::deparse_unit(freqs(to)), mode = "standard")
  x
}

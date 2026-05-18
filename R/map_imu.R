#' Apply a function to each element of an IMU vector
#'
#' @description
#' This function provides a general framework to apply an arbitrary function
#' to each element of an IMU vector while providing access to each
#' element's burst, frequency, and start time metadata.
#'
#' Note that some [common operations][explore-functions] have already been implemented as stand-alone
#' functions.
#'
#' @details
#' This function behaves similarly to the [purrr::map()] family of functions.
#' However, `map_imu()` only matches arguments by name, not position. Thus,
#' the input to `.f` must use the specified terminology (`.br`, `.fq`,
#' and/or `.st`) to access specific data from each element.
#' For a given vector `x`:
#'
#' - `.br` accesses each element of the list returned by [bursts()]
#' - `.fq` accesses each element of the vector returned by [freqs()]
#' - `.st` accesses each element of the vector returned by [starts()]
#'
#' @param x An IMU vector (e.g. `acc`, `mag`, `gyro`)
#' @param .f A function to be applied to each element of `x`. This can be
#'   supplied in one of the following ways:
#'   - A named function
#'   - An anonymous function (e.g., `function(.br) nrow(.br) / .fq`)
#'   - A formula (e.g., `~ nrow(.br) / .fq`)
#'
#'   In all cases, use `.br` to refer to the burst matrix of each element, `.fq`
#'   to refer to the frequency of each element, and `.st` to refer to the
#'   start time of each element. See examples.
#' @param simplify Logical. If `TRUE`, attempts to simplify the output to a
#'   vector. Otherwise, the output will be a list. If the output cannot be
#'   simplified while maintaining a one-to-one correspondence with the input,
#'   an error will be thrown.
#' @param .progress Whether to show a progress bar. Use `TRUE` to turn on a
#'   basic progress bar, use a string to give it a name.
#'
#' @export
#'
#' @examples
#' a <- acc_example()
#'
#' # Use `.br` to access the burst matrix for each element:
#' n_samp <- map_imu(a, function(.br) nrow(.br))
#'
#' n_samp
#'
#' # Use `.fq` to access the frequency value for each element:
#' burst_len <- map_imu(a, function(.br, .fq) nrow(.br) / .fq)
#'
#' burst_len
#'
#' # Use `.st` to access the start time for each element:
#' burst_end <- map_imu(
#'   a,
#'   function(.br, .fq, .st) as.numeric(nrow(.br) / .fq) + .st
#' )
#'
#' burst_end
#'
#' # You can also provide a separately defined function
#' get_burst_end <- function(.br, .fq, .st, offset = 2) {
#'   as.numeric(nrow(.br) / .fq) + .st + offset
#' }
#'
#' map_imu(a, get_burst_end)
#'
#' map_imu(a, function(.br, .fq, .st) get_burst_end(.br, .fq, .st, offset = 5))
#'
#' # Use simplify to reduce to a vector format:
#' map_imu(a, get_burst_end, simplify = TRUE)
#'
#' # Note that this will fail if the result cannot be simplified to the same
#' # length as the input vector
#' try(
#'   map_imu(a, function(.br) .br, simplify = TRUE)
#' )
map_imu <- function(x, .f, simplify = FALSE, .progress = FALSE) {
  assert_imu(x)

  f <- as_imu_mapper(.f)

  out <- purrr::pmap(
    list(
      .br = bursts(x),
      .fq = freqs(x),
      .st = starts(x)
    ),
    function(.br, .fq, .st) {
      f(.br = .br, .fq = .fq, .st = .st)
    },
    .progress = .progress
  )

  if (simplify) {
    out <- purrr::list_simplify(out)
  }

  out
}

as_imu_mapper <- function(.f) {
  if (rlang::is_formula(.f)) {
    # Build function with custom arg names to refer to record fields
    f <- rlang::new_function(
      args = rlang::pairlist2(.br = , .fq = , .st = ),
      body = rlang::f_rhs(.f),
      env  = rlang::f_env(.f)
    )
  } else if (is.function(.f)) {
    # If a function already, convert to format where not all named args
    # are required. `force()` ensures correct enclosing env for f
    force(.f)

    f <- function(.br, .fq, .st) {
      available <- list(.br = .br, .fq = .fq, .st = .st)
      do.call(.f, available[names(available) %in% names(formals(.f))])
    }
  } else {
    rlang::abort("`.f` must be a function or a formula.")
  }

  f
}

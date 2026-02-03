
#' Functions to explore an `acc` vector
#'
#' @param x an acc vector
#'
#' @rdname explore-functions
#' @export
#' @examples
#' x <- acc(
#'   bursts = list(
#'     cbind(x = sin(1:30 / 10), y = cos(1:30 / 10), z = 1),
#'     cbind(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
#'   ),
#'   frequency = units::as_units(c(20, 30), "Hz")
#' )
#' x <- c(x, NA)
#' n_axis(x)
#' n_samples(x)
#' is_uniform(x)
#' length(x)
#' is.na(x)
#' na.omit(x)
#'  y <- acc(
#'   bursts = list(
#'     cbind(x = sin(1:20 / 10), y = cos(1:20 / 10)),
#'     cbind(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
#'   ),
#'   frequency = units::as_units(c(20, 20), "Hz")
#' )
#' is_uniform(y)
n_axis <- function(x) {
  r <- rep(NA_integer_, vec_size(x))
  r[!is.na(x)] <- purrr::map_int(bursts(x[!is.na(x)]), ncol)
  r
}

#' @export
#' @rdname explore-functions
n_samples <- function(x) {
  r <- rep(NA_integer_, vec_size(x))
  r[!is.na(x)] <- purrr::map_int(bursts(x[!is.na(x)]), nrow)
  r
}

#' @export
#' @importFrom stats na.omit
#' @rdname explore-functions
is_uniform<-function(x){
  # TODO check units are same?
  all(duplicated(na.omit(n_samples(x)))[-1])&&
  all(duplicated(na.omit(n_axis(x)))[-1]) &&
  all(duplicated(na.omit(freqs(x)))[-1]) &&
  all(duplicated(purrr::map(bursts(x[!is.na(x)]), colnames))[-1]) &&
  all(duplicated(purrr::map_lgl(bursts(x[!is.na(x)]), inherits, "units"))[-1])
}

# TODO: I don't love this construction since it isn't entirely clear what
# criteria are being used to determine if an entry is NA. Previoulsy this was
# essentially determining whether an acc was NA based on the frequency and start, as
# the bursts were NULL and therefore ignored. Now at least we explicitly check
# bursts for NA, since this seems most relevant. But it would be reasonable
# for a user to expect that this is checking the full acc record. We should
# probably base this and na.omit on vctrs completion criteria and export
# as different functions.
#' @rdname explore-functions
#' @export
is.na.acc <- function(x) {
  purrr::map_lgl(
    bursts(x),
    function(b) rlang::is_empty(b) | rlang::is_na(b)
  )
}

#' @export
#' @rdname explore-functions
bursts <- function(x) {
  field(x, "bursts")
}

#' @export
#' @rdname explore-functions
freqs <- function(x) {
  field(x, "frequency")
}

#' @export
#' @rdname explore-functions
starts <- function(x) {
  field(x, "start")
}

#' @export
#' @rdname explore-functions
burst_dur <- function(x) {
  bursts <- bursts(x)
  frq <- freqs(x)
  
  dur <- purrr::map2_dbl(
    bursts,
    frq,
    function(x, y) (nrow(x) %||% NA) / y
  )
  
  units::set_units(dur, "s") # not sure if this is guaranteed...but units gives 1/Hz by default which is annoying.
}

#' @export
#' @rdname explore-functions
burst_n <- function(x) {
  purrr::map_int(bursts(x), function(b) nrow(b) %||% NA_integer_)
}

#' @export
#' @rdname explore-functions
is_acc <- function(x) {
  inherits(x, "acc")
}

# TODO finish function and export?
static_acc <- function(x) {
  # should this return a list or a dataframe
  # TODO fix NA
  lapply(bursts(x)[!is.na(x)], colMeans)
}

#' @export
vec_cast.acc.acc <- function(x, to, ...) {
  x
}

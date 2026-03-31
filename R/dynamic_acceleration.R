#' @export
vedba <- function(x) {
  na_units <- detect_burst_units(x)
  map_acc(x, function(.br) vedba_(.br, na_units = na_units), simplify = TRUE)
}

#' @export
odba <- function(x) {
  na_units <- detect_burst_units(x)
  map_acc(x, function(.br) odba_(.br, na_units = na_units), simplify = TRUE)
}

vedba_ <- function(b, na_units = NULL, ...) {
  if (rlang::is_empty(b) || rlang::is_na(b)) {
    na_val <- NA_real_
    
    if (!is.null(na_units)) {
      na_val <- units::set_units(na_val, na_units, mode = "standard")
    }
    
    return(na_val)
  }

  if (inherits(b, "units")) {
    u <- units(b)
    b <- t(b) - units::set_units(colMeans(b), u, mode = "standard")

    vedba <- mean(sqrt(colSums(b ^ 2)))
    vedba <- units::set_units(vedba, u, mode = "standard")
  } else {
    b <- t(b) - colMeans(b)
    vedba <- mean(sqrt(colSums(b ^ 2)))
  }

  vedba
}

odba_ <- function(b, na_units = NULL, ...) {
  if (rlang::is_empty(b) || rlang::is_na(b)) {
    na_val <- NA_real_
    
    if (!is.null(na_units)) {
      na_val <- units::set_units(na_val, na_units, mode = "standard")
    }
    
    return(na_val)
  }

  if (inherits(b, "units")) {
    u <- units(b)
    b <- t(b) - units::set_units(colMeans(b), u, mode = "standard")

    odba <- mean(colSums(abs(b)))
    odba <- units::set_units(odba, u, mode = "standard")
  } else {
    b <- t(b) - colMeans(b)
    odba <- mean(colSums(abs(b)))
  }

  odba
}

# Helper to identify the primary units for the bursts in an acc vector
# acc vector bursts can have heterogeneous units, but units can be coerced
# to each other when simplifying computed values to a vector, as is done in DBA
# functions. This finds the first entry in an acc with units.
detect_burst_units <- function(x) {
  b <- purrr::detect(bursts(x), ~ inherits(.x, "units"))
  
  if (!is.null(b)) {
    u <- units(b)
  } else {
    u <- NULL
  }
  
  u
}

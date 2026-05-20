#' Summarize an IMU vector
#'
#' @description
#' Provides a diagnostic overview of an IMU vector (`acc`, `mag`, `gyro`),
#' including axis combinations, frequencies, burst sizes, timing, and value
#' ranges.
#'
#' @param object An IMU vector.
#' @param ... Ignored.
#'
#' @returns An `imu_summary` object.
#'
#' @export
#'
#' @examples
#' a <- acc_example()
#' summary(a)
summary.imu <- function(object, ...) {
  x <- object[!is.na(object)]
  n <- length(object)
  n_na <- sum(is.na(object))
  n_valid <- length(x)
  
  out <- list(sensor = class(object)[1], n = n, n_na = n_na)

  if (n_valid == 0) {
    return(new_imu_summary(out))
  }
  
  br <- bursts(x)
  br_cols <- purrr::map(br, colnames)
  
  # Axes
  axis_combos <- purrr::map_chr(
    br_cols, 
    function(b) paste(b, collapse = "")
  )
  
  out$axes <- sort(table(axis_combos), decreasing = TRUE)
  
  # Frequency
  f <- freqs(x)
  out$freq_unit <- units::deparse_unit(f)
  out$freqs <- as.numeric(f)
  
  # Samples
  out$samples <- n_samples(x)
  
  # Duration
  dur <- burst_dur(x)
  out$dur_unit <- units::deparse_unit(dur)
  out$durations <- as.numeric(dur)
  
  # Start times
  st <- starts(x)
  st <- sort(st[!is.na(st)])
  
  if (length(st) > 0) {
    out$start_range <- range(st)
  } else {
    out$start_range <-  NULL
  }
  
  out$start_tz <- attr(st, "tzone") %||% "UTC"
  
  # Intervals
  if (length(st) > 1) {
    out$intervals <- as.numeric(diff(st), units = "secs")
  }
  
  # Value ranges
  all_axes <- unique(unlist(br_cols))
  all_axes <- all_axes[order(match(all_axes, c("X", "Y", "Z")))]
  out$value_ranges <- purrr::map(
    stats::setNames(all_axes, all_axes), 
    function(ax) {
      vals <- unlist(
        purrr::map(
          br, 
          function(b) {
            if (ax %in% colnames(b)) {
              as.numeric(b[, ax]) 
            } else {
              NULL
            }
          }
        )
      )
      
      if (length(vals) > 0) {
        range(vals)
      }
    }
  )
  
  # Units
  bu_all <- imu_units(x)
  out$imu_units <- unique(na.omit(bu_all))
  out$has_unitless <- any(is.na(bu_all))
  
  new_imu_summary(out)
}

#' @export
print.imu_summary <- function(x, ...) {
  # Header
  if (x$n_na > 0) {
    na_note <- paste0(" (", format_count(x$n_na), " NA)") 
  } else {
    na_note <- "" 
  }
  
  cat(format_count(x$n), " ", x$sensor, " bursts", na_note, "\n", sep = "")
  
  if (is.null(x$axes)) {
    return(invisible(x))
  }
  
  # Time range
  if (!is.null(x$start_range)) {
    if (nzchar(x$start_tz)) { 
      tz_label <- paste0(" ", x$start_tz)
    } else {
      tz_label <- "" 
    }
    
    cat(paste0("from ", format(x$start_range[1]), " to ", format(x$start_range[2]), tz_label), "\n")
  }
  
  cat("\n")
  
  # Axes
  axis_parts <- paste0(
    names(x$axes), " (", format_count(as.integer(x$axes)), ")"
  )
  cat("Axes:", paste(axis_parts, collapse = ", "), "\n")
  
  # Frequency
  cat("Frequencies:", format_range(x$freqs, x$freq_unit), "\n")
  
  # Samples
  cat("Samples per burst:", format_range(x$samples), "\n")
  
  # Duration
  cat("Durations:", format_range(x$durations, x$dur_unit), "\n")
  
  # Intervals
  if (!is.null(x$intervals)) {
    cat("\n")
    cat("Intervals:\n")
    cat("  Median:", format_val(stats::median(x$intervals), "s"), "\n")
    cat("  Range: ", format_range(c(min(x$intervals), max(x$intervals)), "s"), "\n")
  }
  
  # Value ranges
  if (length(x$value_ranges) > 0) {
    cat("\n")
    cat("Value ranges:\n")
    for (ax in names(x$value_ranges)) {
      r <- x$value_ranges[[ax]]
      cat("  ", paste0(ax, ":"), paste0("[", format_num(r[1]), ", ", format_num(r[2]), "]"), "\n")
    }
  }
  
  # Units
  cat("\n")
  labels <- character(0)
  if (length(x$imu_units) > 0) labels <- paste0("[", x$imu_units, "]")
  if (isTRUE(x$has_unitless)) labels <- c(labels, "[no units]")
  if (length(labels) == 0) labels <- "[no units]"
  cat("Units:", paste(labels, collapse = ", "), "\n")
  
  invisible(x)
}

#' Plot an `imu_summary`
#'
#' Produces a multi-panel histogram of the distributions stored in an
#' `imu_summary` object.
#'
#' @param x An `imu_summary` object (returned by [summary.imu()]).
#' @param ... Passed to [graphics::hist()].
#'
#' @returns Invisibly returns `x`.
#'
#' @export
#'
#' @examples
#' s <- summary(acc_example())
#' plot(s)
plot.imu_summary <- function(x, ...) {
  if (is.null(x$axes)) {
    message("Nothing to plot (no non-NA bursts).")
    return(invisible(x))
  }
  
  panels <- list(
    Frequency = list(
      data = x$freqs,
      xlab = paste0("Frequency [", x$freq_unit, "]")
    ),
    `Samples per burst` = list(
      data = x$samples,
      xlab = "Samples per burst"
    ),
    Duration = list(
      data = x$durations,
      xlab = paste0("Duration [", x$dur_unit, "]")
    )
  )
  
  if (!is.null(x$intervals)) {
    panels$Interval <- list(
      data = x$intervals,
      xlab = "Interval [s]"
    )
  }
  
  np <- length(panels)
  nc <- min(np, 2)
  nr <- ceiling(np / nc)
  
  oldpar <- graphics::par(mfrow = c(nr, nc), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(oldpar))
  
  for (nm in names(panels)) {
    p <- panels[[nm]]
    graphics::hist(p$data, main = nm, xlab = p$xlab, col = "grey80", border = "white", ...)
  }
  
  invisible(x)
}

new_imu_summary <- function(x) {
  structure(x, class = c("imu_summary", class(x)))
}

format_count <- function(x) {
  format(x, big.mark = ",", trim = TRUE)
}

format_num <- function(x) {
  format(round(x, 2), trim = TRUE, nsmall = 0)
}

format_val <- function(x, unit = NULL) {
  out <- format_num(x)
  if (!is.null(unit)) out <- paste0(out, " [", unit, "]")
  out
}

format_range <- function(x, unit = NULL) {
  mn <- format_num(min(x))
  mx <- format_num(max(x))
  if (!is.null(unit)) {
    paste0(mn, " [", unit, "] -- ", mx, " [", unit, "]")
  } else {
    paste0(mn, " -- ", mx)
  }
}

format_top_n <- function(x, unit = NULL, n = 5) {
  tbl <- sort(table(x), decreasing = TRUE)
  vals <- as.numeric(names(tbl))
  counts <- as.integer(tbl)
  truncated <- length(tbl) > n
  vals <- vals[seq_len(min(n, length(vals)))]
  counts <- counts[seq_len(min(n, length(counts)))]
  labels <- if (!is.null(unit)) {
    paste0(format_num(vals), " [", unit, "]")
  } else {
    format_num(vals)
  }
  parts <- paste0(labels, " (", format_count(counts), ")")
  out <- paste(parts, collapse = ", ")
  if (truncated) out <- paste0(out, ", ...")
  out
}

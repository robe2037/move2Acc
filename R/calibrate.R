#' Calibrate accelerometer axis parameters
#'
#' Computes per-axis offset and slope values from raw calibration data,
#' returning named vectors that can be passed directly to [eobs_transform()].
#'
#' When `df` is a data.frame with `acc_x`, `acc_y`, and/or `acc_z` columns,
#' each column is calibrated as a separate axis. This is convenient for
#' calibrating from tabular data files. When `df` is `NULL`, individual axis
#' vectors are passed via `acc_x`, `acc_y`, `acc_z`.
#'
#' @param df A data.frame with `acc_x`, `acc_y`, and/or `acc_z` columns.
#'   If `NULL`, at least one of `acc_x`, `acc_y`, or `acc_z` must be provided.
#' @param acc_x,acc_y,acc_z Numeric vectors of raw calibration values for
#'   each axis. Ignored when `df` is provided.
#' @param ... Additional arguments passed to [stable_calibration_segs()].
#'
#' @return A list with elements:
#'   \describe{
#'     \item{offset}{Named numeric vector with per-axis zero-g offsets}
#'     \item{slope}{Named numeric vector with per-axis raw-to-g slopes}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Calibrate from a data.frame with acc_x/acc_y/acc_z columns
#' df <- sim_calibration_data(seed = 42)
#' cal <- calibrate_tag(df)
#'
#' # Build a tag_config and transform
#' tc <- as_tag_config(cal, tag_id = "4501", tag_gen = 3)
#' eobs_transform(acc_example(), tag_config = tc)
#'
#' # Or pass offset/slope directly
#' eobs_transform(acc_example(), tag_gen = 3, offset = cal$offset, slope = cal$slope)
#' }
calibrate_tag <- function(df = NULL, 
                          acc_x = NULL, 
                          acc_y = NULL, 
                          acc_z = NULL, 
                          ...) {
  if (!rlang::is_null(df)) {
    acc_cols <- list(X = "acc_x", Y = "acc_y", Z = "acc_z")
    
    present <- purrr::keep(acc_cols, function(col) col %in% colnames(df))
    
    if (length(present) == 0) {
      rlang::abort(
        c(
          "No acceleration columns found in `df`.",
          i = "Expected one or more of: `acc_x`, `acc_y`, `acc_z`."
        )
      )
    }
    
    axes <- purrr::map(present, function(col) df[[col]])
  } else {
    axes <- purrr::compact(list(X = acc_x, Y = acc_y, Z = acc_z))
    
    if (length(axes) == 0) {
      rlang::abort(
        "At least one of `acc_x`, `acc_y`, or `acc_z` must be provided."
      )
    }
  }
  
  calibration <- purrr::map(axes, function(axis) calibrate_axis(axis, ...))
  
  list(
    offset = purrr::map_dbl(calibration, function(x) x$offset),
    slope  = purrr::map_dbl(calibration, function(x) x$slope)
  )
}

#' Calculate calibration offset and slope from stable segments
#'
#' Computes the offset (zero-g level) and slope (raw-to-g conversion factor)
#' from stable calibration segments for a single axis. The offset is the
#' midpoint between the maximum and minimum stable means (the +1g and -1g
#' orientations), and the slope converts the raw value range to \[-1, 1\] g.
#'
#' @param x A numeric vector of raw calibration values or a data.frame as
#'   returned by [stable_calibration_segs()].
#' @param ... If `x` is a numeric vector, additional arguments passed
#'   to [stable_calibration_segs()] (e.g., `sd_threshold`, `min_stable_samples`,
#'   `window`).
#'
#' @return A list with elements `offset` and `slope`.
calibrate_axis <- function(x, ...) {
  if (is.numeric(x)) {
    x <- stable_calibration_segs(x, ...)
  }
  
  if (nrow(x) < 2) {
    rlang::abort(
      c(
        "At least 2 stable segments are needed to calibrate.",
        i = paste0("Found ", nrow(x), " stable segment(s).")
      )
    )
  }
  
  max_med <- max(x$med_val)
  min_med <- min(x$med_val)
  range_val <- max_med - min_med

  if (range_val < 1) {
    rlang::abort(
      c(
        "Segment medians span too narrow a range to calibrate.",
        i = paste0(
          "Range: ", round(range_val, 2),
          " (min=", round(min_med, 2), ", max=", round(max_med, 2), ")."
        ),
        i = "Ensure the sensor was rotated through both +1g and -1g for this axis."
      )
    )
  }

  list(
    offset = (max_med + min_med) / 2,
    slope  = 2 / range_val
  )
}

#' Find stable segments in a calibration recording
#'
#' Uses rolling-window standard deviation to identify segments of stable
#' acceleration values in a calibration recording where the sensor is held
#' stationary in multiple orientations. Contiguous runs of low-variability
#' samples are detected and filtered by length.
#'
#' @param x Numeric vector of raw acceleration values from a single axis
#' @param sd_threshold Maximum rolling standard deviation for a sample to be
#'   considered stable. Default 10.
#' @param min_stable_samples Minimum number of samples for a stable segment.
#'   Default 200.
#' @param window Rolling window size in samples. Default 50.
#'
#' @return A data.frame with columns: `start`, `end`, `n`, `med_val`, `sd_val`
#' @export
stable_calibration_segs <- function(x,
                                    sd_threshold = 10,
                                    min_stable_samples = 200,
                                    window = 50) {
  rsd <- rolling_sd(x, window)
  is_stable <- !is.na(rsd) & rsd < sd_threshold
  runs <- find_logical_runs(is_stable)
  runs <- runs[runs$n >= min_stable_samples, ]
  
  if (nrow(runs) == 0) {
    return(
      data.frame(
        start = integer(0),
        end = integer(0),
        n = integer(0),
        med_val = numeric(0),
        sd_val = numeric(0)
      )
    )
  }
  
  do.call(
    rbind,
    purrr::map2(runs$start, runs$end, function(s, e) unpack_segment(x, s, e))
  )
}

#' Convert calibration output to a tag_config row
#'
#' Converts the output of [calibrate_tag()] into a single-row data.frame
#' compatible with the `tag_config` argument of [eobs_transform()]. Multiple
#' tags can be assembled by row-binding the results.
#'
#' @param calibration A list with `offset` and `slope` elements as returned
#'   by [calibrate_tag()].
#' @param tag_id Tag identifier (character or numeric).
#' @param tag_gen Tag generation (numeric), passed to [eobs_transform()].
#' @param sensitivity Sensitivity setting, either `"low"` (default) or
#'   `"high"`.
#'
#' @return A single-row data.frame with columns `tag_id`, `tag_gen`,
#'   `sensitivity`, `offset_x`, `offset_y`, `offset_z`, `slope_x`, `slope_y`,
#'   `slope_z`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Single-tag workflow
#' cal <- calibrate_tag(df)
#' tc <- as_tag_config(cal, tag_id = "4501", tag_gen = 3)
#'
#' # Multi-tag assembly
#' tc <- rbind(
#'   as_tag_config(cal1, tag_id = "4501", tag_gen = 3),
#'   as_tag_config(cal2, tag_id = "2100", tag_gen = 1)
#' )
#' eobs_transform(acc_data, tag_config = tc)
#' }
as_tag_config <- function(calibration, tag_id, tag_gen, sensitivity = "low") {
  rlang::arg_match(sensitivity, c("low", "high"))
  
  offset <- recycle_to_axes(calibration$offset, default = NA_real_)
  slope  <- recycle_to_axes(calibration$slope, default = NA_real_)
  
  data.frame(
    tag_id = tag_id,
    tag_gen = tag_gen,
    sensitivity = sensitivity,
    offset_x = offset[["X"]],
    offset_y = offset[["Y"]],
    offset_z = offset[["Z"]],
    slope_x = slope[["X"]],
    slope_y = slope[["Y"]],
    slope_z = slope[["Z"]],
    stringsAsFactors = FALSE
  )
}

#' Plot calibration segments
#'
#' Diagnostic plot showing detected stable segments overlaid on the raw
#' calibration signal. Stable segments are shaded and their medians shown
#' as dashed horizontal lines.
#'
#' @param x Vector of raw acceleration values recorded during calibration.
#' @param frq Sample rate in Hz, used to create a time axis. Default 1
#'   (x-axis shows sample index).
#' @param ... Additional arguments passed to [stable_calibration_segs()].
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_calibration <- function(x, frq = 1, ...) {
  rlang::check_installed("ggplot2", reason = "to plot calibration segments")
  rlang::check_installed("scales", reason = "to plot calibration segments")
  
  segments <- stable_calibration_segs(x, ...)
  
  time <- seq(0, length(x) - 1) / frq
  
  df <- data.frame(time = time, x = x)
  
  # Label each sample as stable or unstable
  df$segment_id <- "Unstable"
  
  if (nrow(segments) > 0) {
    for (i in seq_len(nrow(segments))) {
      idx <- segments$start[i]:segments$end[i]
      df$segment_id[idx] <- paste0(
        "Stable ", i, " (median=", round(segments$med_val[i], 3), ")"
      )
    }
  }
  
  stable_labels <- unique(df$segment_id[df$segment_id != "Unstable"])
  
  # Assign a unique group to each contiguous run so that non-adjacent
  # unstable sections are drawn as separate lines
  df$group <- cumsum(c(1, diff(as.integer(factor(df$segment_id))) != 0))
  
  shade_df <- data.frame(
    xmin = time[segments$start],
    xmax = time[segments$end]
  )
  
  x_lab <- ifelse(frq == 1, "Sample", "Time (s)")
  
  ggplot2::ggplot(df, ggplot2::aes(x = .data[["time"]], y = .data[["x"]])) +
    ggplot2::geom_rect(
      data = shade_df,
      ggplot2::aes(
        xmin = .data[["xmin"]], 
        xmax = .data[["xmax"]],
        ymin = -Inf, 
        ymax = Inf
      ),
      inherit.aes = FALSE,
      fill = "steelblue", 
      alpha = 0.12
    ) +
    ggplot2::geom_line(
      ggplot2::aes(colour = .data[["segment_id"]], group = .data[["group"]]),
      linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      data = segments,
      ggplot2::aes(yintercept = .data[["med_val"]]),
      linetype = "dashed", 
      linewidth = 0.5, 
      colour = "grey40"
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        stats::setNames(scales::hue_pal()(length(stable_labels)), stable_labels),
        "Unstable" = "gray"
      )
    ) +
    ggplot2::labs(
      title  = "Accelerometer calibration segmentation",
      x      = x_lab,
      y      = "Acceleration (raw)",
      colour = "Segment"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Simulate 3-axis accelerometer calibration data
#'
#' Generates synthetic calibration data for all three axes simultaneously,
#' simulating a sensor rotated through 6 physically realistic orientations
#' (±X, ±Y, ±Z). At each orientation, exactly one axis reads ±1g while the
#' other two read ~0g. Output values are in raw 12-bit ADC scale (0--4095).
#'
#' @param n_per_stable Number of samples per stable orientation. Default 500.
#' @param noise_sd Sensor noise standard deviation in raw units during stable
#'   periods. Default 8.
#' @param frq Sample rate in Hz. Default 28.
#' @param offset Per-axis zero-g level in raw units (the reading when the axis
#'   is perpendicular to gravity). Scalar (recycled to all axes) or named
#'   vector with elements `X`, `Y`, and/or `Z`. Default 0.
#' @param scale Per-axis difference between the +1g and -1g readings in raw
#'   units. Controls each axis's sensitivity independently. Scalar (recycled)
#'   or named vector. Default 740 (i.e., +1g reads offset + 370, -1g reads
#'   offset - 370).
#' @param seed Random seed for reproducibility. Default `NULL` (no seed set).
#'
#' @return A data.frame with columns `time`, `acc_x`, `acc_y`, `acc_z`, and
#'   `true_label`.
sim_calibration_data <- function(n_per_stable = 500,
                                 noise_sd = 8,
                                 frq = 28,
                                 offset = 0,
                                 scale = 740,
                                 seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  if (length(offset) == 1 & rlang::is_null(names(offset))) {
    offset <- stats::setNames(rep(offset, 3), AXES)
  }
  
  if (length(scale) == 1 & rlang::is_null(names(offset))) {
    scale <- stats::setNames(rep(scale, 3), AXES)
  }
  
  # Resolve per-axis offset and scale (sensitivity)
  offset <- recycle_to_axes(offset, default = 0)
  scale  <- recycle_to_axes(scale, default = 740)
  sensitivity <- scale / 2
  
  # 6 physical orientations: each row is one placement (X, Y, Z) in g-space
  orientations <- matrix(
    c(
      1,  0,  0,
      -1,  0,  0,
      0,  1,  0,
      0, -1,  0,
      0,  0,  1,
      0,  0, -1
    ),
    ncol = 3, byrow = TRUE,
    dimnames = list(NULL, AXES)
  )
  
  # Randomize the order the sensor is rotated through
  orientations <- orientations[sample(nrow(orientations)), ]
  
  # Convert orientation matrix from g-space to raw: offset + g * sensitivity
  raw_orientations <- sweep(
    sweep(orientations, 2, sensitivity, `*`),
    2, offset, `+`
  )
  
  acc_x <- numeric(0)
  acc_y <- numeric(0)
  acc_z <- numeric(0)
  labels <- character(0)
  
  for (i in seq_len(nrow(raw_orientations))) {
    ori <- raw_orientations[i, ]
    
    # Stable segment — each axis gets noise around its raw orientation value
    acc_x <- c(acc_x, stats::rnorm(n_per_stable, mean = ori[["X"]], sd = noise_sd))
    acc_y <- c(acc_y, stats::rnorm(n_per_stable, mean = ori[["Y"]], sd = noise_sd))
    acc_z <- c(acc_z, stats::rnorm(n_per_stable, mean = ori[["Z"]], sd = noise_sd))
    
    g_ori <- orientations[i, ]
    labels <- c(labels, rep(
      paste0("stable_", i, "_X", g_ori[["X"]], "_Y", g_ori[["Y"]], "_Z", g_ori[["Z"]]),
      n_per_stable
    ))
    
    # Rotation transient (omit after last orientation)
    if (i < nrow(raw_orientations)) {
      n_rot <- sample(30:150, 1)
      ori_nxt <- raw_orientations[i + 1, ]
      turb_sd <- noise_sd * 15
      
      sweep_x <- seq(ori[["X"]], ori_nxt[["X"]], length.out = n_rot)
      sweep_y <- seq(ori[["Y"]], ori_nxt[["Y"]], length.out = n_rot)
      sweep_z <- seq(ori[["Z"]], ori_nxt[["Z"]], length.out = n_rot)
      
      acc_x <- c(acc_x, sweep_x + stats::rnorm(n_rot, sd = turb_sd))
      acc_y <- c(acc_y, sweep_y + stats::rnorm(n_rot, sd = turb_sd))
      acc_z <- c(acc_z, sweep_z + stats::rnorm(n_rot, sd = turb_sd))
      labels <- c(labels, rep(paste0("rotation_", i), n_rot))
    }
  }
  
  data.frame(
    time = seq(0, length(acc_x) - 1) / frq,
    acc_x = acc_x,
    acc_y = acc_y,
    acc_z = acc_z,
    true_label = labels
  )
}

# O(n) rolling SD using cumulative sums. Returns NA for the first
# `window - 1` positions.
rolling_sd <- function(x, window) {
  n <- length(x)
  
  if (n < window || all(is.na(x))) {
    return(rep(NA_real_, n))
  }

  na_idx <- which(is.na(x))
  
  if (length(na_idx) > 0) {
    rlang::warn(
      paste0(
        length(na_idx), " NA value(s) in input; ",
        "interpolating before computing rolling SD."
      )
    )
    good <- which(!is.na(x))
    x <- stats::approx(good, x[good], xout = seq_len(n), rule = 2)$y
  }

  cs  <- cumsum(x)
  cs2 <- cumsum(x^2)
  
  sum_x  <- cs[window:n] - c(0, cs[seq_len(n - window)])
  sum_x2 <- cs2[window:n] - c(0, cs2[seq_len(n - window)])
  
  variance <- (sum_x2 - sum_x^2 / window) / (window - 1)
  # Clamp floating-point noise to zero before sqrt
  variance[variance < 0] <- 0
  
  c(rep(NA_real_, window - 1), sqrt(variance))
}

# Find contiguous runs of TRUE in a logical vector.
# Returns a data.frame with `start`, `end`, `n`.
find_logical_runs <- function(x) {
  r <- rle(x)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  keep <- which(r$values)
  
  data.frame(
    start = starts[keep],
    end = ends[keep],
    n = r$lengths[keep]
  )
}

unpack_segment <- function(x, start, end) {
  seg <- x[start:end]
  
  data.frame(
    start = start,
    end = end,
    n = end - start + 1,
    med_val = median(seg, na.rm = TRUE),
    sd_val = sd(seg, na.rm = TRUE)
  )
}

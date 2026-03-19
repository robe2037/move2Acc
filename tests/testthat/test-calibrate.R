# ---- Deterministic test data ------------------------------------------------

# A clean single-axis signal: two stable plateaus separated by a noisy gap.
# Plateau 1: 300 samples at 1000, Plateau 2: 300 samples at -1000
# Gap: 100 samples of linearly spaced values (simulates rotation)
calibration_signal <- function() {
  c(rep(1000, 300), seq(1000, -1000, length.out = 100), rep(-1000, 300))
}

# A segments data.frame as returned by stable_calibration_segs(), for testing
# functions that accept pre-computed segments
mock_segments <- function() {
  data.frame(
    start    = c(1L, 401L),
    end      = c(300L, 700L),
    n        = c(300L, 300L),
    med_val  = c(1000, -1000),
    sd_val   = c(0, 0)
  )
}

# ---- rolling_sd() -----------------------------------------------------------

test_that("rolling_sd returns correct values for a known sequence", {
  x <- as.numeric(1:5) + c(0.1, 0.3, 0.02, 0.4, 0.01)
  result <- rolling_sd(x, window = 3)

  expect_length(result, 5)
  expect_true(all(is.na(result[1:2])))
  expect_equal(result[3], sd(x[1:3]))
  expect_equal(result[4], sd(x[2:4]))
  expect_equal(result[5], sd(x[3:5]))
})

test_that("rolling_sd returns all NA when input is shorter than window", {
  result <- rolling_sd(c(1, 2), window = 5)

  expect_length(result, 2)
  expect_true(all(is.na(result)))
})

test_that("rolling_sd returns 0 for constant input", {
  result <- rolling_sd(rep(42, 20), window = 5)

  expect_equal(result[5:20], rep(0, 16))
})

test_that("rolling_sd warns and interpolates over NA values", {
  x <- as.numeric(101:120)
  x[c(5, 10)] <- NA

  expect_warning(result <- rolling_sd(x, window = 3), "2 NA value")
  expect_length(result, 20)
  
  # No NAs beyond the leading window-1 positions
  expect_false(any(is.na(result[3:20])))
})

test_that("rolling_sd with NAs still allows segment detection", {
  x <- calibration_signal()
  x[c(50, 150)] <- NA

  expect_warning(
    segs <- stable_calibration_segs(x, min_stable_samples = 50),
    "2 NA value"
  )
  expect_equal(nrow(segs), 2)
  expect_setequal(segs$med_val, c(-1000, 1000))
  expect_true(all(!is.na(segs$sd_val)))
})

# ---- find_logical_runs() ----------------------------------------------------

test_that("find_logical_runs identifies contiguous TRUE runs", {
  x <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  result <- find_logical_runs(x)

  expect_equal(nrow(result), 2)
  expect_equal(result$start, c(2, 6))
  expect_equal(result$end, c(4, 7))
  expect_equal(result$n, c(3, 2))
})

test_that("find_logical_runs returns empty data.frame when all FALSE", {
  result <- find_logical_runs(rep(FALSE, 5))

  expect_equal(nrow(result), 0)
  expect_named(result, c("start", "end", "n"))
})

test_that("find_logical_runs handles all TRUE", {
  result <- find_logical_runs(rep(TRUE, 5))

  expect_equal(nrow(result), 1)
  expect_equal(result$start, 1)
  expect_equal(result$end, 5)
  expect_equal(result$n, 5)
})

# ---- unpack_segment() -------------------------------------------------------

test_that("unpack_segment computes correct median and sd", {
  x <- c(10, 20, 30, 40, 50)
  result <- unpack_segment(x, 2, 4)

  expect_equal(result$start, 2)
  expect_equal(result$end, 4)
  expect_equal(result$n, 3)
  expect_equal(result$med_val, median(x[2:4]))
  expect_equal(result$sd_val, sd(x[2:4]))
})

# ---- stable_calibration_segs() ----------------------------------------------

test_that("stable_calibration_segs finds two plateaus in clean signal", {
  x <- calibration_signal()
  segs <- stable_calibration_segs(x, sd_threshold = 10, min_stable_samples = 50)

  expect_s3_class(segs, "data.frame")
  expect_named(segs, c("start", "end", "n", "med_val", "sd_val"))
  expect_equal(nrow(segs), 2)
  expect_setequal(segs$med_val, c(-1000, 1000))
  expect_true(all(segs$n >= 50))
  expect_true(all(segs$sd_val <= 10))
})

test_that("stable_calibration_segs returns empty df when no segments qualify", {
  # All noise — nothing stable
  x <- stats::rnorm(500, sd = 100)
  segs <- stable_calibration_segs(x, sd_threshold = 1, min_stable_samples = 200)

  expect_s3_class(segs, "data.frame")
  expect_equal(nrow(segs), 0)
  expect_named(segs, c("start", "end", "n", "med_val", "sd_val"))
})

test_that("stable_calibration_segs respects min_stable_samples", {
  x <- calibration_signal()

  # With a threshold larger than the plateau length, nothing should pass
  segs <- stable_calibration_segs(x, sd_threshold = 10, min_stable_samples = 500)
  expect_equal(nrow(segs), 0)
})

test_that("stable_calibration_segs respects sd_threshold", {
  x <- calibration_signal()

  # With threshold = 0, only perfectly constant regions pass, but the rolling
  # SD of a constant region is 0 which is not < 0
  segs <- stable_calibration_segs(x, sd_threshold = 0, min_stable_samples = 50)
  expect_equal(nrow(segs), 0)
})

test_that("stable_calibration_segs respects window parameter", {
  x <- calibration_signal()

  # A very large window eats into the stable region, reducing segment length
  segs_small <- stable_calibration_segs(x, window = 10, min_stable_samples = 50)
  segs_large <- stable_calibration_segs(x, window = 100, min_stable_samples = 50)

  expect_true(all(segs_small$n > segs_large$n))
})

# ---- calibrate_axis() -------------------------------------------------------

test_that("calibrate_axis computes correct offset and slope from segments df", {
  segs <- mock_segments()
  cal <- calibrate_axis(segs)

  expect_equal(cal$offset, 0)
  expect_equal(cal$slope, 2 / 2000)
})

test_that("calibrate_axis works from raw numeric vector", {
  x <- calibration_signal()
  cal <- calibrate_axis(x, min_stable_samples = 50)

  expect_equal(cal$offset, 0)
  expect_equal(cal$slope, 0.001)
})

test_that("calibrate_axis errors when segment medians span too narrow a range", {
  # Identical medians → range = 0
  narrow <- data.frame(
    start = c(1L, 301L), end = c(300L, 600L), n = c(300L, 300L),
    med_val = c(5, 5), sd_val = c(1, 1)
  )
  expect_error(calibrate_axis(narrow), "too narrow")

  # Range of 0.5 → still below threshold of 1
  narrow2 <- data.frame(
    start = c(1L, 301L), end = c(300L, 600L), n = c(300L, 300L),
    med_val = c(5, 5.5), sd_val = c(1, 1)
  )
  expect_error(calibrate_axis(narrow2), "too narrow")

  # Range of 2 → above threshold, should succeed
  ok <- data.frame(
    start = c(1L, 301L), end = c(300L, 600L), n = c(300L, 300L),
    med_val = c(5, 7), sd_val = c(1, 1)
  )
  result <- calibrate_axis(ok)
  expect_equal(result$offset, 6)
  expect_equal(result$slope, 1)
})

test_that("calibrate_axis errors with fewer than 2 segments", {
  one_seg <- mock_segments()[1, ]
  expect_error(calibrate_axis(one_seg), "At least 2 stable segments")
})

test_that("calibrate_axis errors with zero segments", {
  empty_seg <- mock_segments()[0, ]
  expect_error(calibrate_axis(empty_seg), "At least 2 stable segments")
})

# ---- calibrate_tag() -------------------------------------------------------

test_that("calibrate_tag works with a data.frame input", {
  df <- data.frame(
    acc_x = calibration_signal(),
    acc_y = calibration_signal() * 2
  )
  cal <- calibrate_tag(df, min_stable_samples = 50)

  expect_named(cal, c("offset", "slope"))
  expect_named(cal$offset, c("X", "Y"))
  expect_named(cal$slope, c("X", "Y"))
  expect_equal(cal$offset[["X"]], 0)
  expect_equal(cal$offset[["Y"]], 0)
  expect_equal(cal$slope[["X"]], 0.001)
  expect_equal(cal$slope[["Y"]], 0.0005)
})

test_that("calibrate_tag works with individual axis vectors", {
  sig <- calibration_signal()
  cal <- calibrate_tag(acc_x = sig, min_stable_samples = 50)

  expect_named(cal$offset, "X")
  expect_equal(cal$offset[["X"]], 0)
})

test_that("calibrate_tag errors when df has no acc columns", {
  bad_df <- data.frame(a = 1:10, b = 11:20)
  expect_error(calibrate_tag(bad_df), "No acceleration columns")
})

test_that("calibrate_tag errors when no axes provided", {
  expect_error(calibrate_tag(), "At least one")
})

# ---- as_tag_config() --------------------------------------------------------

test_that("as_tag_config returns correct single-row data.frame", {
  cal <- list(
    offset = c(X = 10, Y = 20, Z = 30),
    slope  = c(X = 0.001, Y = 0.002, Z = 0.003)
  )
  tc <- as_tag_config(cal, tag_id = "4501", tag_gen = 3)

  expect_s3_class(tc, "data.frame")
  expect_equal(nrow(tc), 1)
  expect_equal(tc$tag_id, "4501")
  expect_equal(tc$tag_gen, 3)
  expect_equal(tc$sensitivity, "low")
  expect_equal(tc$offset_x, 10)
  expect_equal(tc$offset_y, 20)
  expect_equal(tc$offset_z, 30)
  expect_equal(tc$slope_x, 0.001)
  expect_equal(tc$slope_y, 0.002)
  expect_equal(tc$slope_z, 0.003)
})

test_that("as_tag_config fills NA for missing axes", {
  cal <- list(
    offset = c(X = 10),
    slope  = c(X = 0.001)
  )
  tc <- as_tag_config(cal, tag_id = "1", tag_gen = 1)
  
  expect_true(all(c("offset_y", "offset_z", "slope_y", "slope_z") %in% colnames(tc)))
  expect_true(is.na(tc$offset_y))
  expect_true(is.na(tc$offset_z))
  expect_true(is.na(tc$slope_y))
  expect_true(is.na(tc$slope_z))
})

test_that("as_tag_config rejects invalid sensitivity", {
  cal <- list(offset = c(X = 0), slope = c(X = 1))
  expect_error(as_tag_config(cal, tag_id = "1", tag_gen = 1, sensitivity = "medium"))
})

# ---- sim_calibration_data() -------------------------------------------------

test_that("sim_calibration_data returns expected columns and dimensions", {
  df <- sim_calibration_data(n_per_stable = 100, seed = 1)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("time", "acc_x", "acc_y", "acc_z", "true_label") %in% names(df)))

  # 6 stable orientations × 100 samples + 5 rotation transients
  n_stable <- 6 * 100
  expect_true(nrow(df) > n_stable)
})

test_that("sim_calibration_data is reproducible with seed", {
  df1 <- sim_calibration_data(n_per_stable = 100, seed = 99)
  df2 <- sim_calibration_data(n_per_stable = 100, seed = 99)

  expect_identical(df1, df2)
})

# ---- Full pipeline round-trip -----------------------------------------------

test_that("calibrate_tag recovers known offset and slope from simulated data", {
  df <- sim_calibration_data(
    n_per_stable = 500,
    noise_sd = 5,
    offset = c(X = 2048, Y = 100, Z = -10),
    scale = c(Y = 300, Z = 600),
    seed = 42
  )
  
  cal <- calibrate_tag(df)

  # offset should be near `offset`, slope near 2/`scale` ≈ 0.002703
  expect_equal(cal$offset[["X"]], 2048, tolerance = 0.1)
  expect_equal(cal$offset[["Y"]], 100, tolerance = 0.1)
  expect_equal(cal$offset[["Z"]], -10, tolerance = 0.1)
  expect_equal(cal$slope[["X"]], 2 / 740, tolerance = 0.001)
  expect_equal(cal$slope[["Y"]], 2 / 300, tolerance = 0.001)
  expect_equal(cal$slope[["Z"]], 2 / 600, tolerance = 0.001)
})

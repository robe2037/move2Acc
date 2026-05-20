test_that("summary returns imu_summary object", {
  s <- summary(acc_example())
  expect_s3_class(s, "imu_summary")
  expect_equal(s$n, 2)
  expect_equal(s$n_na, 0)
})

test_that("summary prints header with burst count and NAs", {
  out <- capture.output(print(summary(c(acc_example(), NA))))
  expect_true(any(grepl("3 acc bursts", out)))
  expect_true(any(grepl("1 NA", out)))
})

test_that("summary prints axis combinations inline", {
  out <- capture.output(print(summary(acc_example())))
  axes_line <- out[grepl("Axes:", out)]
  expect_length(axes_line, 1)
  expect_match(axes_line, "XY")
  expect_match(axes_line, "XYZ")
})

test_that("summary handles empty acc", {
  s <- summary(acc())
  expect_s3_class(s, "imu_summary")
  expect_equal(s$n, 0)
  out <- capture.output(print(s))
  expect_true(any(grepl("0 acc bursts", out)))
})

test_that("summary shows units when present", {
  out <- capture.output(print(summary(units::set_units(acc_example(), "m/s^2"))))
  expect_true(any(grepl("Units:.*m/s\\^2", out)))
})

test_that("summary shows no units when bursts are unitless", {
  out <- capture.output(print(summary(acc_example())))
  expect_true(any(grepl("Units:.*no units", out)))
})

test_that("summary captures value ranges", {
  s <- summary(acc_example())
  expect_named(s$value_ranges, c("X", "Y", "Z"))
  expect_length(s$value_ranges$X, 2)
})

test_that("summary captures intervals", {
  s <- summary(acc_example())
  expect_true(!is.null(s$intervals))
  expect_length(s$intervals, 1)
})

test_that("Can filter acc by frequency criteria", {
  # 4 bursts with frequencies: 2 Hz, NA (single sample), 2 Hz, 4/3 Hz
  a <- acc(
    c(
      acc_burst_example(1:5),
      acc_burst_example(99),
      acc_burst_example(6:16),
      acc_burst_example(17:69)
    ),
    frequency = units::set_units(c(2, NA, 2, 4 / 3), "Hz"),
    start = as.POSIXct(c(1, 4, 6, 10.5), tz = "UTC")
  )

  expect_identical(a, filter_freq(a, keep_na = TRUE))

  expect_equal(
    freqs(filter_freq(a)),
    units::set_units(c(2, 2, 4 / 3), "Hz")
  )
  expect_equal(
    freqs(filter_freq(a, min_freq = 1.5)),
    units::set_units(c(2, 2), "Hz")
  )
  expect_equal(
    freqs(filter_freq(a, max_freq = 1.5)),
    units::set_units(4 / 3, "Hz")
  )
  expect_equal(
    freqs(filter_freq(a, min_freq = 1.5, keep_na = TRUE)),
    units::set_units(c(2, NA, 2), "Hz")
  )
  expect_identical(
    filter_freq(a, min_freq = units::set_units(1.5, "Hz")),
    filter_freq(a, min_freq = units::set_units(1.5 / 1000, "kHz"))
  )
})

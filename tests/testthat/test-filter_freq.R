test_that("Can filter acc by frequency criteria", {
  t <- data.frame(
    id = 1,
    acceleration_x = 1:69,
    acceleration_y = 1:69,
    acceleration_z = 1:69,
    timestamp = as.POSIXct(
      c(
        seq(1, 3, by = 0.5), 4, 5.5, 
        seq(6, 10, by = 0.5), seq(10.5, 50, by = 0.75)
      ), 
      "UTC"
    ),
    x = 1, 
    y = 1
  )
  
  m <- move2::mt_as_move2(
    t,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  a <- as_acc(m)
  
  expect_identical(a, filter_freq(a, keep_na = TRUE))
  
  expect_equal(
    freqs(filter_freq(a)), 
    round(units::set_units(c(2, 2, 4 / 3), "Hz"), 4)
  )
  expect_equal(
    freqs(filter_freq(a, min_freq = 1.5)),
    round(units::set_units(c(2, 2), "Hz"), 4)
  )
  expect_equal(
    freqs(filter_freq(a, max_freq = 1.5)),
    round(units::set_units(4 / 3, "Hz"), 4)
  )
  expect_equal(
    freqs(filter_freq(a, min_freq = 1.5, keep_na = TRUE)),
    round(units::set_units(c(2, NA, 2), "Hz"), 4)
  )
  expect_identical(
    filter_freq(a, min_freq = units::set_units(1.5, "Hz")),
    filter_freq(a, min_freq = units::set_units(1.5 / 1000, "kHz"))
  )
})

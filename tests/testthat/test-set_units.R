test_that("set_burst_units.acc attaches units to unitless bursts", {
  a <- acc_example()
  a_u <- set_burst_units(a, "m/s^2")

  expect_true(inherits(bursts(a_u)[[1]], "units"))
  expect_true(inherits(bursts(a_u)[[2]], "units"))
  expect_identical(
    as.character(units(bursts(a_u)[[1]])),
    "m/s^2"
  )
})

test_that("set_burst_units.acc converts between compatible units", {
  a <- set_burst_units(acc_example(), "m/s^2")
  a_sff <- set_burst_units(a, "standard_free_fall")

  expect_identical(
    as.character(units(bursts(a_sff)[[1]])),
    "standard_free_fall"
  )

  # Values should be converted (1 standard_free_fall = 9.80665 m/s^2)
  expect_equal(
    as.numeric(bursts(a_sff)[[1]]),
    as.numeric(bursts(a)[[1]]) / 9.80665,
    tolerance = 1e-6
  )
})

test_that("set_burst_units.acc preserves numeric values when attaching units", {
  a <- acc_example()
  a_u <- set_burst_units(a, "m/s^2")

  expect_equal(
    as.numeric(bursts(a_u)[[1]]),
    as.numeric(bursts(a)[[1]])
  )
})

test_that("set_burst_units.acc handles NA elements", {
  a <- acc_example()
  a_na <- c(a[1], acc(list(NULL), units::set_units(NA, "Hz")), a[2])
  a_u <- set_burst_units(a_na, "m/s^2")

  expect_identical(is.na(a_u), c(FALSE, TRUE, FALSE))
  expect_true(is.null(bursts(a_u)[[2]]))
  expect_true(inherits(bursts(a_u)[[1]], "units"))
})

test_that("set_burst_units.acc preserves acc structure", {
  a <- acc_example()
  a_u <- set_burst_units(a, "m/s^2")

  expect_length(a_u, length(a))
  expect_identical(freqs(a_u), freqs(a))
  expect_identical(starts(a_u), starts(a))
  expect_identical(colnames(bursts(a_u)[[1]]), colnames(bursts(a)[[1]]))
})

test_that("set_burst_units.acc rejects dimensionally incompatible units", {
  a <- acc_example()

  expect_error(set_burst_units(a, "kg"), "not valid for `acc` vector")
  expect_error(set_burst_units(a, "m"), "not valid for `acc` vector")
  expect_error(set_burst_units(a, "not-a-unit"), "not valid for `acc` vector")
})

test_that("set_burst_units.mag attaches and converts magnetic flux density units", {
  m <- mag(
    list(cbind(X = 1:5, Y = 1:5, Z = 1:5)),
    units::set_units(20, "Hz")
  )

  m_t <- set_burst_units(m, "tesla")
  # udunits normalizes "tesla" to "T" in its canonical display form
  expect_identical(as.character(units(bursts(m_t)[[1]])), "T")

  m_ut <- set_burst_units(m_t, "uT")
  expect_equal(
    as.numeric(bursts(m_ut)[[1]]),
    as.numeric(bursts(m_t)[[1]]) * 1e6
  )
})

test_that("set_burst_units.mag rejects dimensionally incompatible units", {
  m <- mag(
    list(cbind(X = 1:3, Y = 1:3, Z = 1:3)),
    units::set_units(20, "Hz")
  )

  expect_error(set_burst_units(m, "m/s^2"), "not valid for `mag` vector")
  expect_error(set_burst_units(m, "kg"), "not valid for `mag` vector")
})

test_that("set_burst_units.gyro attaches and converts angular velocity units", {
  g <- gyro(
    list(cbind(X = 1:5, Y = 1:5, Z = 1:5)),
    units::set_units(20, "Hz")
  )

  g_rad <- set_burst_units(g, "rad/s")
  expect_identical(as.character(units(bursts(g_rad)[[1]])), "rad/s")

  g_rpm <- set_burst_units(g_rad, "rad/min")
  expect_equal(
    as.numeric(bursts(g_rpm)[[1]]),
    as.numeric(bursts(g_rad)[[1]]) * 60
  )
})

test_that("set_burst_units.gyro rejects dimensionally incompatible units", {
  g <- gyro(
    list(cbind(X = 1:3, Y = 1:3, Z = 1:3)),
    units::set_units(20, "Hz")
  )

  expect_error(set_burst_units(g, "m/s^2"), "not valid for `gyro` vector")
  expect_error(set_burst_units(g, "tesla"), "not valid for `gyro` vector")
})

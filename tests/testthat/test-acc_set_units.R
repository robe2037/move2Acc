test_that("acc_set_units attaches units to unitless bursts", {
  a <- acc_example()
  a_u <- acc_set_units(a, "m/s^2")

  expect_true(inherits(bursts(a_u)[[1]], "units"))
  expect_true(inherits(bursts(a_u)[[2]], "units"))
  expect_identical(
    as.character(units(bursts(a_u)[[1]])),
    "m/s^2"
  )
})

test_that("acc_set_units converts between compatible units", {
  a <- acc_set_units(acc_example(), "m/s^2")
  a_sff <- acc_set_units(a, "standard_free_fall")

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

test_that("acc_set_units preserves numeric values when attaching units", {
  a <- acc_example()
  a_u <- acc_set_units(a, "m/s^2")

  expect_equal(
    as.numeric(bursts(a_u)[[1]]),
    as.numeric(bursts(a)[[1]])
  )
})

test_that("acc_set_units handles NA elements", {
  a <- acc_example()
  a_na <- c(a[1], acc(list(NULL), units::set_units(NA, "Hz")), a[2])
  a_u <- acc_set_units(a_na, "m/s^2")

  expect_identical(is.na(a_u), c(FALSE, TRUE, FALSE))
  expect_true(is.null(bursts(a_u)[[2]]))
  expect_true(inherits(bursts(a_u)[[1]], "units"))
})

test_that("acc_set_units preserves acc structure", {
  a <- acc_example()
  a_u <- acc_set_units(a, "m/s^2")

  expect_length(a_u, length(a))
  expect_identical(freqs(a_u), freqs(a))
  expect_identical(starts(a_u), starts(a))
  expect_identical(colnames(bursts(a_u)[[1]]), colnames(bursts(a)[[1]]))
})

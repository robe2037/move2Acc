test_that("acc map function generator always returns a function", {
  expect_true(is.function(as_imu_mapper(~ .br)))
  expect_true(is.function(as_imu_mapper(function(.br) .br)))
})

test_that("returned function always accepts .br, .fq, .st by name", {
  f <- as_imu_mapper(function(.br) .br)
  expect_no_error(f(.br = 1, .fq = 2, .st = 3))
})

test_that("reserved function arguments interpreted correctly", {
  a <- acc_example()
  
  expect_equal(
    map_imu(a, function(.br) nrow(.br) * 2, simplify = TRUE),
    purrr::map_dbl(bursts(a), nrow) * 2
  )
  expect_equal(
    map_imu(a, function(.fq) as.numeric(.fq) + 10, simplify = TRUE),
    purrr::map_dbl(freqs(a), as.numeric) + 10
  )
  expect_equal(
    map_imu(a, function(.st) as.numeric(.st + 1), simplify = TRUE),
    purrr::map_dbl(starts(a), as.numeric) + 1
  )
})

test_that("Can use multiple reserved function args", {
  a <- acc_example()
  
  expect_identical(
    map_imu(a, function(.br, .fq) nrow(.br) + as.numeric(.fq)),
    purrr::map2(bursts(a), freqs(a), ~ nrow(.x) + as.numeric(.y))
  )
  expect_identical(
    map_imu(a, function(.fq, .st) .st + as.numeric(.fq)),
    purrr::map2(starts(a), freqs(a), ~ .x + as.numeric(.y))
  )
  expect_identical(
    map_imu(a, function(.br, .fq, .st) nrow(.br) + as.numeric(.fq) + .st),
    purrr::pmap(
      list(bursts(a), freqs(a), starts(a)), 
      function(b, f, s) nrow(b) + as.numeric(f) + s
    )
  )
})

test_that("Can use formula syntax", {
  a <- acc_example()
  
  expect_identical(
    map_imu(acc_example(), function(.br) .br * 2),
    map_imu(acc_example(), ~ .br * 2)
  )
  expect_identical(
    map_imu(a, function(.fq, .st) .st + as.numeric(.fq)),
    map_imu(a, ~ .st + as.numeric(.fq))
  )
  expect_identical(
    map_imu(a, function(.br, .fq, .st) nrow(.br) + as.numeric(.fq) + .st),
    map_imu(a, ~ nrow(.br) + as.numeric(.fq) + .st)
  )
})

test_that("formula captures enclosing environment correctly", {
  scale <- 100

  expect_identical(
    map_imu(acc_example(), function(.br) .br * scale),
    purrr::map(bursts(acc_example()), ~ .x * 100)
  )
})

test_that("Can call predefined function", {
  my_fun <- function(.br, .st) nrow(.br) * 2 + .st
  a <- acc_example()
  
  expect_identical(
    map_imu(a, my_fun),
    purrr::map2(bursts(a), starts(a), function(x, y) nrow(x) * 2 + y)
  )
})

test_that("Can call predefined function with additional arguments", {
  my_fun <- function(.br, scale = 1) nrow(.br) * scale
  a <- acc_example()
  
  expect_identical(
    map_imu(a, my_fun),
    purrr::map(bursts(a), ~ nrow(.x) * 1)
  )
  expect_identical(
    map_imu(a, ~ my_fun(.br, scale = 2)),
    purrr::map(bursts(a), ~ nrow(.x) * 2)
  )
  expect_identical(
    map_imu(a, ~ my_fun(.br, scale = .fq)),
    purrr::map2(bursts(a), freqs(a), ~ nrow(.x) * .y)
  )
})

test_that("Can simplify", {
  expect_identical(
    map_imu(acc_example(), ~ .fq, simplify = TRUE),
    freqs(acc_example())
  )
  expect_error(
    map_imu(acc_example(), ~ .br, simplify = TRUE),
    "must have size"
  )
})

test_that("Error on unrecognized arg names (do not match by position)", {
  my_fun <- function(x, y) nrow(x) + y
  
  expect_error(map_imu(acc_example(), function(x) nrow(x)))
  expect_error(map_imu(acc_example(), my_fun))
})

test_that("map_imu() basic requirements", {
  expect_error(map_imu(acc_example(), "foobar"), "must be a function or")
  expect_error(map_imu(acc_example(), NULL), "must be a function or")
  expect_error(map_imu(1, length), "must be an `acc`,.*`gyro` vector")
})

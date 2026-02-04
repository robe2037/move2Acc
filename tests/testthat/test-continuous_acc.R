test_that("Can combine adjacent bursts into single burst", {
  d <- albatrosses()

  # Simulate bursts that start at the end point of the previous burst
  move2::mt_time(d) <- seq(
    min(move2::mt_time(d)), 
    by = "12 s",
    length.out = nrow(d)
  )
  
  a <- as_acc(d, merge_continuous = FALSE)
  a2 <- merge_continuous_acc(a)
  
  expect_true(is_acc(a2))
  expect_length(a2, 9)
  
  # Split unmerged into acc groups based on whether the start timestamp plus
  # the burst duration is equal to the next start timestamp (these are records
  # that should have been merged in a2)
  acc_grps <- split(
    a, 
    cumsum(c(TRUE, diff(starts(a)) != as.numeric(burst_dur(a)[-1])))
  )
  
  expect_length(acc_grps, length(a2))
  
  # All start timestamps after merging should correspond to the start timestamp
  # of the first entry in each grouped acc from above
  expect_identical(
    as.POSIXct(
      unname(unlist(purrr::map(acc_grps, function(x) starts(x[1])))), 
      "UTC"
    ),
    starts(a2)
  )
  # Merged bursts should match bursts formed by rbind-ing the grouped bursts
  expect_identical(
    bursts(a2), 
    purrr::map(acc_grps, function(x) do.call(rbind, bursts(x))),
    ignore_attr = TRUE
  )
})

# TODO: this is a big of a clunky test, but it covers many possible
# combinations of issues that would prevent elements from being collapsed
# together. This could be refactored and we could build more atomic
# unit tests for these behaviors by building explicit test cases with 
# acc_burst_example()
test_that("Do not combine bursts with different n axes or frequencies", {
  a <- as_acc(albatrosses_messy(), merge_continuous = FALSE)
  a2 <- merge_continuous_acc(a)
  
  # Hard-coding the split indices that we should expect from the 
  # test data:
  split_i <- c(1, 4, 7, 11, 12, 13, 31, 33, 44)
  
  expect_true(is_acc(a2))
  expect_length(a2, length(split_i))
  
  expect_identical(starts(a2), starts(a)[split_i])
  expect_identical(freqs(a2), freqs(a)[split_i])
  
  # Manually confirming all the groups we expect. Easiest way to be thorough
  # in this case.
  expect_identical(
    bursts(a2)[[1]],
    do.call(rbind, bursts(a)[1:3])
  )
  expect_identical(
    bursts(a2)[[2]],
    do.call(rbind, bursts(a)[4:6])
  )
  expect_identical(
    bursts(a2)[[3]],
    do.call(rbind, bursts(a)[7:10])
  )
  expect_identical(
    bursts(a2)[[4]],
    bursts(a)[[11]]
  )
  expect_identical(
    bursts(a2)[[5]],
    bursts(a)[[12]]
  )
  expect_identical(
    bursts(a2)[[6]],
    do.call(rbind, bursts(a)[13:30])
  )
  expect_identical(
    bursts(a2)[[7]],
    do.call(rbind, bursts(a)[31:32])
  )
  expect_identical(
    bursts(a2)[[8]],
    do.call(rbind, bursts(a)[33:43])
  )
  expect_identical(
    bursts(a2)[[9]],
    do.call(rbind, bursts(a)[44:45])
  )
})

test_that("Don't combine bursts without start time", {
  a <- acc(
    c(acc_burst_example(x = 1:10), acc_burst_example(x = 1:10)),
    frequency = units::set_units(1, "Hz")
  )
  
  expect_identical(a, merge_continuous_acc(a))
})

test_that("Handle empty acc vectors when binding", {
  expect_identical(merge_continuous_acc(acc()), acc())
  expect_identical(merge_continuous_acc(c(acc(), acc())), acc())
})

test_that("Can split acc at a given interval", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  interval <- 0.5
  split <- split_continuous_acc(a, interval = interval)
  
  expect_length(split, units::drop_units(sum(ceiling(burst_dur(a) / interval))))
  expect_true(all(units::drop_units(burst_dur(split)) == interval))
  
  expect_equal(
    purrr::map_int(bursts(split), nrow),
    c(rep(10, 6), rep(20, 2))
  )
  expect_equal(
    do.call(rbind, bursts(split)[1:6]),
    bursts(a)[[1]]
  )
  expect_equal(
    do.call(rbind, bursts(split)[7:8]),
    bursts(a)[[2]]
  )
  expect_equal(
    freqs(split),
    units::set_units(c(rep(20, 6), rep(40, 2)), "Hz")
  )
  expect_identical(
    starts(a)[1] + cumsum(c(0, rep(interval, 5))),
    starts(split)[1:6]
  )
  expect_identical(
    starts(a)[2] + cumsum(c(0, interval)),
    starts(split)[7:8]
  )
})

test_that("Correctly split when burst length not divisible by interval", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  interval <- 0.7
  split <- split_continuous_acc(a, interval = interval)
  dur <- burst_dur(a)
  
  expect_length(split, units::drop_units(sum(ceiling(burst_dur(a) / interval))))
  
  # Bursts should be split into equal time lengths other than for the last
  # element of each split burst, which will capture whatever burst duration remains
  expect_equal(
    units::drop_units(burst_dur(split)),
    c(
      c(rep(interval, dur[1] %/% interval), dur[1] - (interval * dur[1] %/% interval)),
      c(rep(interval, dur[2] %/% interval), dur[2] - (interval * dur[2] %/% interval))
    )
  )
})

test_that("Can recover split continuous data by merging", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  expect_identical(
    merge_continuous_acc(split_continuous_acc(a, interval = 0.5)),
    a
  )
})

test_that("Long intervals do not modify input acc", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  expect_identical(split_continuous_acc(a, interval = max(burst_dur(a))), a)
})

test_that("Can standardize interval units when splitting", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "kHz"), units::set_units(40, "kHz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  # Default should be in 1/frq units
  expect_length(split_continuous_acc(a, interval = 0.5), 8)
  expect_identical(
    split_continuous_acc(a, interval = 0.5),
    split_continuous_acc(a, interval = units::set_units(0.5 / 1000, "s"))
  )
})

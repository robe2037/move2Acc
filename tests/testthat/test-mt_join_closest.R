# Helpers ------------------------------------------------------------------

skip_if_not_installed("move2")

move2_example <- function(id, t, ...) {
  d <- data.frame(
    id = factor(id),
    t = as.POSIXct(t, tz = "UTC"),
    ...,
    lon = seq_along(id),
    lat = seq_along(id)
  )
  
  move2::mt_as_move2(
    d,
    time_column = "t",
    track_id_column = "id",
    coords = c("lon", "lat"), 
    crs = 4326
  )
}

tbl_example <- function(id, t, ...) {
  data.frame(
    id = factor(id),
    t = as.POSIXct(t, tz = "UTC"),
    ...
  )
}

# Tests --------------------------------------------------------------------

test_that("closest_before matches y records at or before x", {
  x <- move2_example(
    id = c("a", "a"),
    t  = c("2024-01-01 10:00", "2024-01-01 11:00")
  )
  y <- tbl_example(
    id = c("a", "a", "a"),
    t  = c("2024-01-01 09:50", "2024-01-01 10:30", "2024-01-01 11:30"),
    val = c(1, 2, 3)
  )

  res <- mt_join_closest(x, y, method = "closest_before")

  expect_equal(res$val[1], 1)
  expect_equal(res$val[2], 2)
})

test_that("closest_after matches y records at or after x", {
  x <- move2_example(
    id = c("a", "a"),
    t  = c("2024-01-01 10:00", "2024-01-01 11:00")
  )
  y <- tbl_example(
    id = c("a", "a", "a"),
    t  = c("2024-01-01 09:50", "2024-01-01 10:30", "2024-01-01 11:30"),
    val = c(1, 2, 3)
  )

  res <- mt_join_closest(x, y, method = "closest_after")

  expect_equal(res$val[1], 2)
  expect_equal(res$val[2], 3)
})

test_that("exact match only joins on identical timestamps", {
  x <- move2_example(
    id = c("a", "a"),
    t  = c("2024-01-01 10:00", "2024-01-01 11:00")
  )
  y <- tbl_example(
    id = c("a", "a"),
    t  = c("2024-01-01 10:00", "2024-01-01 10:30"),
    val = c(1, 2)
  )

  res <- mt_join_closest(x, y, method = "exact")

  expect_equal(res$val[1], 1)
  expect_true(is.na(res$val[2]))
})

test_that("exact timestamps are included in both closest_before and closest_after", {
  x <- move2_example(id = "a", t  = "2024-01-01 10:00")
  y <- tbl_example(id = "a", t  = "2024-01-01 10:00", val = 1)

  res_before <- mt_join_closest(x, y, method = "closest_before")
  res_after  <- mt_join_closest(x, y, method = "closest_after")

  expect_equal(res_before$val, 1)
  expect_equal(res_after$val, 1)
})

test_that("matches are isolated by track ID", {
  x <- move2_example(
    id = c("a", "b"),
    t  = c("2024-01-01 10:00", "2024-01-01 10:00")
  )
  y <- tbl_example(
    id = c("a", "b"),
    t  = c("2024-01-01 09:50", "2024-01-01 09:55"),
    val = c(1, 2)
  )

  res <- mt_join_closest(x, y, method = "closest_before")

  # Track a should match y for track a only
  expect_equal(res$val[res$id == "a"], 1)
  expect_equal(res$val[res$id == "b"], 2)
})

test_that("no match produces NA in left join", {
  x <- move2_example(id = "a", t  = "2024-01-01 10:00")
  y <- tbl_example(id = "a", t  = "2024-01-01 11:00", val = 1)

  res <- mt_join_closest(x, y, method = "closest_before")
  
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$val))
})

test_that("custom column names work", {
  x <- move2_example(id = "a", t = "2024-01-01 10:00")
  
  y <- data.frame(
    track_id = factor("a"),
    time = as.POSIXct("2024-01-01 09:50", tz = "UTC"),
    val = 1
  )

  res <- mt_join_closest(
    x, 
    y,
    track_id_column_y = "track_id",
    time_column_y = "time"
  )
  
  expect_equal(res$val, 1)
})

test_that("default suffix preserves x column names and y gets .y", {
  x <- move2_example(id = "a", t  = "2024-01-01 10:00", val = 10)
  y <- tbl_example(id = "a", t  = "2024-01-01 09:50", val = 20)

  res <- mt_join_closest(x, y)
  
  expect_equal(res$val, 10)
  expect_equal(res$val.y, 20)
  expect_equal(res$t, as.POSIXct("2024-01-01 10:00", tz = "UTC"))
  expect_equal(res$t.y, as.POSIXct("2024-01-01 09:50", tz = "UTC"))
})

test_that("custom suffix that renames x columns drops move2 class", {
  x <- move2_example(id = "a", t  = "2024-01-01 10:00", val = 10)
  y <- tbl_example(id = "a", t  = "2024-01-01 09:50", val = 20)

  res <- mt_join_closest(x, y, suffix = c("_gps", "_acc"))
  
  expect_false(inherits(res, "move2"))
  expect_s3_class(res, "sf")
  expect_equal(res$val_gps, 10)
  expect_equal(res$val_acc, 20)
})

test_that("output preserves move2 class, geometry, and structure from x", {
  x <- move2_example(
    id = c("a", "b"),
    t  = c("2024-01-01 10:00", "2024-01-01 11:00"),
    val_x = c(1, 2)
  )
  y <- tbl_example(
    id = c("a", "b"),
    t  = c("2024-01-01 09:50", "2024-01-01 10:50"),
    val_y = c(10, 20)
  )

  res <- mt_join_closest(x, y)

  expect_s3_class(res, "move2")
  expect_equal(move2::mt_track_id_column(res), "id")
  expect_equal(nrow(res), nrow(x))
  expect_equal(res$geometry, x$geometry)
  expect_true("val_x" %in% names(res))
  expect_true("val_y" %in% names(res))
  expect_equal(res$val_x, c(1, 2))
})

test_that("additional join conditions can be passed via dots", {
  x <- move2_example(
    id = c("a", "a"),
    t  = c("2024-01-01 10:00", "2024-01-01 11:00"),
    sensor = c("gps", "acc")
  )
  
  y <- tbl_example(
    id = c("a", "a"),
    t  = c("2024-01-01 09:50", "2024-01-01 09:50"),
    sensor = c("gps", "acc"),
    val = c(1, 2)
  )

  # Without matching on sensor, both y rows match each x row based on time/ID
  res_no_dots <- mt_join_closest(x, y)
  expect_equal(nrow(res_no_dots), 4)

  # Matching on sensor produces unique matches
  res <- mt_join_closest(x, y, sensor == sensor)
  expect_equal(nrow(res), 2)
  expect_equal(res$val[res$sensor == "gps"], 1)
  expect_equal(res$val[res$sensor == "acc"], 2)
})

test_that("invalid method is rejected", {
  x <- move2_example(id = "a", t = "2024-01-01 10:00")
  y <- tbl_example(id = "a", t = "2024-01-01 10:00", val = 1)

  expect_error(
    mt_join_closest(x, y, method = "foobar"),
    "method"
  )
})

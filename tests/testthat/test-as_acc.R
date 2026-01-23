test_that("Can get acc from burst-format acc data", {
  alb_data <- albatrosses()
  
  acc <- as_acc(alb_data)
  
  i <- which(!is.na(acc))[1]
  
  expect_s3_class(acc, "acc")
  expect_equal(
    colnames(field(acc, "bursts")[[i]]), 
    strsplit(as.character(alb_data$eobs_acceleration_axes[i]), "")[[1]]
  )
  expect_length(acc, nrow(alb_data))
  expect_equal(is.na(acc), is.na(alb_data$eobs_accelerations_raw))
  expect_true(is_uniform(acc))
})

test_that("Can get acc from long-format acc data", {
  gulls_data <- gulls()
  
  acc_i <- which(gulls_data$sensor_type_id == 2365683)
  
  expect_warning(
    acc <- as_acc(gulls_data), 
    "Detected multiple valid acceleration column sets"
  )
  
  expect_s3_class(acc, "acc")
  expect_length(acc, nrow(gulls_data))
  expect_equal(sum(!is.na(acc)), length(unique(parse_bursts(gulls_data)))) 
  expect_equal(is.na(acc[acc_i]), duplicated(parse_bursts(gulls_data)))
  expect_true(is_uniform(acc))
  expect_equal(
    unique(purrr::map(field(acc[!is.na(acc)], "bursts"), colnames))[[1]],
    c("X", "Y", "Z")
  )
})

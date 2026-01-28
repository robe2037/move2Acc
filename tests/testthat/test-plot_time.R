test_that("plot_time", {
  expect_silent(
    graph <- plot_time(acc_example(), Sys.time() + c(0,10))
  )
  expect_s3_class(graph, "dygraphs")
})

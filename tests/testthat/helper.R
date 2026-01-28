acc_example <- function() {
  acc(
    c(acc_burst_example(1:4, 5:8, 9:12), acc_burst_example(1:4, 5:8)), 
    frequency = units::set_units(2:3, "Hz"),
    start = as.POSIXct(c(1, 10))
  )
}

acc_burst_example <- function(x = NULL, y = NULL, z = NULL) {
  vec_size_common(x, y, z)
  new_acc_list(list(do.call(cbind, list(X = x, Y = y, Z = z))))
}

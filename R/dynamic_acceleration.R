#' @export
vedba <- function(x) {
  map_acc(x, function(.br) vedba_(.br), simplify = TRUE)
}

#' @export
odba <- function(x) {
  map_acc(x, function(.br) odba_(.br), simplify = TRUE)
}

# TODO These fail without units. tapply behaves differently, perhaps because
# values are treated as a list and not a vector?
vedba_ <- function(b, ...) {
  b <- t(b) - do.call("c", tapply(t(b), 1:3, mean, simplify = FALSE))
  mean(sqrt(.keep_units_optional(colSums, b ^ 2)))
}

odba_ <- function(b, ...) {
  b <- t(b) - do.call("c", tapply(t(b), 1:3, mean, simplify = FALSE))
  mean(.keep_units_optional(colSums, abs(b)))
}

# internal function to make keep units only function if it was a unit
.keep_units_optional<-function(FUN,x,...){
  if(inherits(x,"units")){
    units::keep_units(FUN, x,...)
  }else{
    do.call(FUN,x,...)
  }
}

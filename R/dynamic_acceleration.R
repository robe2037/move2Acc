#' @export
dynamic_acceleration<-function(x, method=c('odba','vedba')){
  method<-rlang::arg_match(method)
  b <- bursts(x)
  b_centered<-purrr::map(b, ~t(.x)-do.call('c',tapply(t(.x),1:3, mean, simplify=F)))
  switch(method,
      "odba"=purrr::map_vec(b_centered, ~mean(.keep_units_optional(colSums,abs(.x)))),
      "vedba"= purrr::map_vec(b_centered, ~mean(sqrt(.keep_units_optional(colSums,.x^2))))
      )
}

# internal function to make keep units only function if it was a unit
.keep_units_optional<-function(FUN,x,...){
  if(inherits(x,"units")){
    units::keep_units(FUN, x,...)
  }else{
    do.call(FUN,x,...)
  }
}

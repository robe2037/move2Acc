
#' Calculate the peak frequency per axis for acceleration bursts
#'
#' @param x An `acc` vector
#' @param resolution A scalar with the [units][units::units] Hertz
#'
#' @returns returns a list with the same length as `x` with the peak frequency per axis
#'
#' @details
#'
#' To increase the resolution of the result zero padding can be used. This can be controlled using the resolution
#' argument. Note that increasing resolution without increasing the number of samples in a acceleration burst has a
#' limited ability to get closer to the true frequency.
#'
#' @export
#'
#' @examples
#'   a<-acc(list(cbind(z=cos(1:200/(80/(pi*2))),
#'   x=sin(1:200/(5/(pi*2))))), units::set_units(400,'Hz'))
#'   peak_frequency(a)
#'   peak_frequency(a, units::set_units(.25, "Hz"))
#'   # Increasing resolution more
#'   peak_frequency(a, units::set_units(.005, "Hz"))
#'   a<-acc(list(cbind(z=cos(80+1:200/(80/(pi*2))),
#'   x=sin((1:200)/(5/(pi*2))))), units::set_units(400,'Hz'))
#'   peak_frequency(a, units::set_units(.005, "Hz"))



peak_frequency<-function(x, resolution=NA){
  assertthat::assert_that(inherits(x,'acc'))
  s<-!is.na(x)
  res<-rep(list(NULL),length(x))
  if(all(!s)){
    return(res)
  }
  b <- bursts(x[s])
  b <- purrr::map2(b,purrr::map(b, inherits,'units'), ~if(.y){units::drop_units(.x)}else{.x})
  b_centered<-purrr::map2(b,purrr::map(b, colMeans), ~ t((.x))-.y)
  if(!is.na(resolution)){
    to_pad<-units::drop_units(freqs(x[s])/resolution)-n_samples(x[s])
    b_centered<-purrr::map2(b_centered, to_pad, ~cbind(.x,matrix(0, ncol=.y, nrow=nrow(.x))))
  }
  b_mod<-purrr::map(b_centered, ~ do.call(rbind,lapply(apply(.x,1, fft, simplify = F),  Mod))[,1:ceiling(ncol(.x)/2), drop=F])
  peak<- purrr::map(b_mod, ~ apply(.x,1, which.max ))
  peak_freq<-purrr::pmap(list(peak,freqs(x[s]),purrr::map(b_mod, ncol)), ~ (..1-1)*(..2/..3/2))
  res[s]<-peak_freq
  res
}

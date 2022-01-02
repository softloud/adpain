#' Calculate risk difference parameter
#'
#' @param or
#' @param acr
#'
#' @return
#' @export
#'
#' @examples

calc_rd_par <- function(or, acr) {
  acr - (
    or * acr
  )/(
    1 - acr + or * acr
  )
}

#' Number needed to treat
#'
#' NNTs are always rounded up to the nearest whole number.
#'
#' @param rd risk diff calculated with [calc_rd_par]
#'
#' @return
#' @export
#'
#' @examples

nnt <- function(or, acr) {
  abs(1/calc_rd_par(or, acr)) %>% ceiling()
}

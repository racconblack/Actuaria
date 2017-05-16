#' conversion interes mensual a interes anual
#'
#' @param im tasa de interes mensual. 0 < im < 1
#' @return tasa de interes anual
#' @examples
#'  library(Actuaria)
#'  convi(0.002709263)
#' @export


convi <- function(im){
  intan <- ((1+im)^12)-1
  return(intan)
}



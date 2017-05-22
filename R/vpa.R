#' Valor presente Actuarial
#'
#' @param R Suma asegurada
#' @param x Edad Inicial
#' @param n Años Temporalidad
#' @param i tasa de interes. 0 < i < 1
#' @param sex Sexo "M" para masculino, "F" para Femenino
#' @param base Base de datos que contiene lx
#' @return valor presente Actuarial
#' @examples
#' library(Actuaria)
#'
#' #Valor presente actuarial Anual
#' base <- tcm8903
#' vpa(100000000,30,0,0.025,"M",base)
#'
#' #Valor presente actuarial Anual con temporalidad
#' vpa(100000000,23,20,0.025,"F", base)
#'
#'
#' @export


vpa <- function(R,x,n,i,sex,base){
  m <- 0
  SA <- R
  xmin <- min(as.numeric(base[,1])) - 1
  xmax <- max(as.numeric(base[,1]))
  Aprima <- 0
  ifelse(n > 0, until <- (m+n-1), until <- (xmax-x-1))
  for (ik in m:until){
    conta <- (1+i)^(-ik-1)
    kpx <- NULL

    if(sex == "M"){
      kpx = base[(x+ik)-xmin,2]/base[x-xmin,2]} else if(sex == "F"){
        kpx <- base[(x+ik)-xmin,3]/base[x-xmin,3]}


    Aprima <- Aprima + (conta * kpx)
  }
  totprim <- SA * Aprima
  return(totprim)
}

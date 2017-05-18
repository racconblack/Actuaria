#' Calcula la prima de un seguro dotal
#'
#' @param SA Suma asegurada por muerte
#' @param B Suma asegurada por sobrevivencia
#' @param x Edad Inicial segùn tabla TCM-89-03
#' @param n Años Temporalidad
#' @param m Años Diferimiento
#' @param i tasa de interes. 0 < i < 1
#' @param sex Sexo "M" para masculino, "F" para Femenino
#' @param base Base de datos que contiene lx
#' @return prima de un seguro por muerte
#' @examples
#'library(Actuaria)
#'
#'base <- tcm8903
#'
#'primadot(100000000,12000000,55,15,8,0.03,"M",base)
#'
#' @export


primadot <- function(SA,B,x,n,m,i,sex,base){
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

    kpx2 <- NULL
    if(sex == "M"){
      kpx2 = 1- (base[(x+ik+1)-xmin,2]/base[x+ik-xmin,2])} else if(sex == "F"){
        kpx2 <- 1- (base[(x+ik+1)-xmin,3]/base[x+ik-xmin,3])}

    Aprima <- Aprima + (conta * kpx * kpx2)
  }
  totprim <- SA * Aprima

  sobreprima <- 0
  kpsx <- 0

    if(sex == "M"){
      kpxs = base[(x+n+m)-xmin,2]/base[x-xmin,2]} else if(sex == "F"){
        kpxs <- base[(x+n+m)-xmin,3]/base[x-xmin,3]}

  sobreprima <-  (1+i)^(-n-m) * kpxs * B
  dotal <- totprim + sobreprima
  return(dotal)
}

#' Esta función calcula la prima de un seguro por muerte
#'
#' @param SA Suma asegurada
#' @param x Edad Inicial segùn tabla TCM-89-03
#' @param n Años Temporalidad
#' @param m Años Diferimiento
#' @param i tasa de interes. 0 < i < 1
#' @param sex Sexo "M" para masculino, "F" para Femenino
#' @param base Base de datos que contiene lx
#' @return Función kqx
#' @examples
#' library(Actuaria)
#' x <- tcm8903
#' kqx(23,5,"M",x)
#' @export


prima <- function(SA,x,n,m,i,sex,base){
  Aprima <- 0
  ifelse(n > 0, until <- (m+n-1), until <- (100-x-1))
  for (ik in m:until){
    conta <- (1+i)^(-ik-1)
    kpx <- NULL

    if(sex == "M"){
      kpx = base[(x+ik)-19,2]/base[x-19,2]} else if(sex == "F"){
        kpx <- base[(x+ik)-19,3]/base[x-19,3]}

    kpx2 <- NULL
    if(sex == "M"){
      kpx2 = 1- (base[(x+ik+1)-19,2]/base[x+ik-19,2])} else if(sex == "F"){
        kpx2 <- 1- (base[(x+ik+1)-19,3]/base[x+ik-19,3])}

    Aprima <- Aprima + (conta * kpx * kpx2)
  }
  totprim <- SA * Aprima
  return(totprim)
}

#' Esta función calcula kqx
#'
#' @param k Años de Sobrevivencia segùn tabla TCM-89,03
#' @param x Edad Inicial segùn tabla TCM-89-03
#' @param sex Sexo "M" para masculino, "F" para Femenino
#' @param base Base de datos que contiene lx
#' @return Función kqx
#' @examples
#' library(Actuaria)
#' x <- tcm8903
#' kqx(23,5,"M",x)
#' @export



kqx <- function(x,k, sex, base){
  if(x < 20 | x > 100){cat("Digite una edad x entre 20 y 100")}
  if(x+k < 20 | x+k > 100){cat("Digite una edad x+k entre 20 y 100")}
  if(!sex %in% c("M","F") ){cat("Digite un Sexo Valido")}
  kpx <- NULL
  if(sex == "M"){
    kpx = 1- (base[(x+k)-19,2]/base[x-19,2])
    return(kpx)} else if(sex == "F"){
      kpx <- 1- (base[(x+k)-19,3]/base[x-19,3])
      return(kpx)}
}

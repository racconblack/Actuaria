#' prima de un seguro para meses (interpolacion Lineal o hiperbolica)
#'
#' @param SA Suma asegurada
#' @param x Edad Inicial segùn tabla TCM-89-03 (En meses)
#' @param n Meses Temporalidad
#' @param m Meses Diferimiento
#' @param i tasa de interes (Efectiva Anual). 0 < i < 1. Si tiene la tasa de interes mensual utilize la función interes para saber su equivalente anual
#' @param sex Sexo "M" para masculino, "F" para Femenino
#' @param interpolar Metodo de interpolacion "lin" para lineal o "hip" para hiperbolica
#' @param base Base de datos que contiene lx
#' @return prima de un seguro para meses
#' @examples
#'  library(Actuaria)
#'  base <-  tcm8903
#'
#'  Interpolación Lineal
#'  mensual(SA = 150000000, x = 360,n = 0,m = 0,i = 0.033,sex = "M",interpolar = "lin",base )
#'
#'  Interpolacion hipoerbolica
#'  mensual(SA = 150000000, x = 360,n = 0,m = 0,i = 0.033,sex = "M",interpolar = "hip",base )
#' @export

mensual <- function(SA,x,n,m,i,sex,interpolar = "lin",base){

  i <- ((1+i)^(1/12))-1

  inf <- as.numeric(base[1,1])*12
  sup <- as.numeric(base[81,1])*12
  base2 <- data.frame(mes= inf:sup, lineal = 0, hiperbolic = 0)
  base2$edad <- base2$mes%/%12
  cont <- rep(0:11,(as.numeric(base[81,1])-as.numeric(base[1,1])))
  cont[length(cont)+1] <- 0
  base2$div <- cont
  base2$s <- base2$div/12

  base$edad <- as.numeric(base$edad)
  base2 <- merge(base2,base, by = "edad", all.x = T)
  base2$linealy <- 0
  base2$hiperbolicy <- 0
  rows <- nrow(base2)
  base2[(rows+1):(rows+12),] <- base2[rows,]

  for (ik in 1:nrow(base2)){
    base2[ik,"lineal"] <- base2[ik,"lx"] *  (1-base2[ik,"s"]) + base2[ik+12,"lx"] * base2[ik,"s"]
    base2[ik,"linealy"] <- base2[ik,"ly"] *  (1-base2[ik,"s"]) + base2[ik+12,"ly"] * base2[ik,"s"]

    base2[ik,"hiperbolic"] <- (base2[ik,"lx"] * base2[ik+12,"lx"]) / (base2[ik+12,"lx"] + base2[ik,"s"]* (base2[ik,"lx"]-base2[ik+12,"lx"]))
    base2[ik,"hiperbolicy"] <- (base2[ik,"ly"] * base2[ik+12,"ly"]) / (base2[ik+12,"ly"] + base2[ik,"s"]* (base2[ik,"ly"]-base2[ik+12,"ly"]))

    }
base2 <- base2[1:rows,]

if (interpolar == "lin"){
  Aprima <- 0
  ifelse(n > 0, until <- (m+n-1), until <- (1200-x-1))
  for (ik in m:until){
 # ik <- until
    conta <- (1+i)^(-ik-1)
    kpx <- NULL

    if(sex == "M"){
      kpx = base2[(x+ik)-239,"lineal"]/base2[x-239,"lineal"]} else if(sex == "F"){
        kpx <- base2[(x+ik)-239,"linealy"]/base2[x-239,"linealy"]}

    kpx2 <- NULL
    if(sex == "M"){
      kpx2 = 1- (base2[(x+ik+1)-239,"lineal"]/base2[x+ik-239,"lineal"])} else if(sex == "F"){
        kpx2 <- 1- (base2[(x+ik+1)-239,"linealy"]/base2[x+ik-239,"linealy"])}

    Aprima <- Aprima + (conta * kpx * kpx2)
  }
  totprim <- SA * Aprima
  return(totprim)

} else if (interpolar == "hip"){

  Aprima <- 0
  ifelse(n > 0, until <- (m+n-1), until <- (1200-x-1))
  for (ik in m:until){
    conta <- (1+i)^(-ik-1)
    kpx <- NULL

    if(sex == "M"){
      kpx = base2[(x+ik)-239,"hiperbolic"]/base2[x-239,"hiperbolic"]} else if(sex == "F"){
        kpx <- base2[(x+ik)-239,"hiperbolicy"]/base2[x-239,"hiperbolicy"]}

    kpx2 <- NULL
    if(sex == "M"){
      kpx2 = 1- (base2[(x+ik+1)-239,"hiperbolic"]/base2[x+ik-239,"hiperbolic"])} else if(sex == "F"){
        kpx2 <- 1- (base2[(x+ik+1)-239,"hiperbolicy"]/base2[x+ik-239,"hiperbolicy"])}

    Aprima <- Aprima + (conta * kpx * kpx2)
  }
  totprim <- SA * Aprima
  return(totprim)

}

}



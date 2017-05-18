#' prima de un seguro dotal para meses (interpolacion Lineal o hiperbolica)
#'
#' @param SA Suma asegurada
#' @param B Suma asegurada dotal
#' @param x Edad Inicial segun tabla TCM-89-03 (En meses)
#' @param n Meses Temporalidad
#' @param m Meses Diferimiento
#' @param i tasa de interes (Efectiva Anual). 0 < i < 1. Si tiene la tasa de interes mensual utilize la función convi para saber su equivalente anual
#' @param sex Sexo "M" para masculino, "F" para Femenino
#' @param interpolar Metodo de interpolacion "lin" para lineal o "hip" para hiperbolica
#' @param base Base de datos que contiene lx
#' @return prima de un seguro para meses
#' @examples
#'  library(Actuaria)
#'  base <-  tcm8903
#'
#'  Interpolación Lineal
#'  mensualdot(100000000,10000000,788,159,24,0.0345,"M", "lin", base)
#'
#'  Interpolacion hiperbolica
#'  mensualdot(100000000,10000000,788,159,24,0.0345,"M", "hip", base)
#' @export

mensualdot <- function(SA,B,x,n,m,i,sex,interpolar = "lin",base){
  xmin <- (min(as.numeric(base[,1]))*12) - 1
  xmax <- max(as.numeric(base[,1]))*12
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
    ifelse(n > 0, until <- (m+n-1), until <- (xmax-x-1))
    for (ik in m:until){
      # ik <- until
      conta <- (1+i)^(-ik-1)
      kpx <- NULL

      if(sex == "M"){
        kpx = base2[(x+ik)-xmin ,"lineal"]/base2[x-xmin ,"lineal"]} else if(sex == "F"){
          kpx <- base2[(x+ik)-xmin ,"linealy"]/base2[x-xmin ,"linealy"]}

      kpx2 <- NULL
      if(sex == "M"){
        kpx2 = 1- (base2[(x+ik+1)-xmin ,"lineal"]/base2[x+ik-xmin ,"lineal"])} else if(sex == "F"){
          kpx2 <- 1- (base2[(x+ik+1)-xmin ,"linealy"]/base2[x+ik-xmin ,"linealy"])}

      Aprima <- Aprima + (conta * kpx * kpx2)
    }
    totprim <- SA * Aprima


    if(sex == "M"){
      kpxs = base2[(x+n+m)-xmin ,"lineal"]/base2[x-xmin ,"lineal"]} else if(sex == "F"){
        kpxs <- base2[(x+n+m)-xmin ,"linealy"]/base2[x-xmin ,"linealy"]}

    sobreprima <-  (1+i)^(-n-m) * kpxs * B
    dotal <- totprim + sobreprima
    return(dotal)

  } else if (interpolar == "hip"){

    Aprima <- 0
    ifelse(n > 0, until <- (m+n-1), until <- (xmax-x-1))
    for (ik in m:until){
      conta <- (1+i)^(-ik-1)
      kpx <- NULL

      if(sex == "M"){
        kpx = base2[(x+ik)-xmin ,"hiperbolic"]/base2[x-xmin ,"hiperbolic"]} else if(sex == "F"){
          kpx <- base2[(x+ik)-xmin ,"hiperbolicy"]/base2[x-xmin ,"hiperbolicy"]}

      kpx2 <- NULL
      if(sex == "M"){
        kpx2 = 1- (base2[(x+ik+1)-xmin ,"hiperbolic"]/base2[x+ik-xmin ,"hiperbolic"])} else if(sex == "F"){
          kpx2 <- 1- (base2[(x+ik+1)-xmin ,"hiperbolicy"]/base2[x+ik-xmin ,"hiperbolicy"])}

      Aprima <- Aprima + (conta * kpx * kpx2)
    }
    totprim <- SA * Aprima

    sobreprima <- 0
    kpsx <- 0

    if(sex == "M"){
      kpxs = base2[(x+n+m)-xmin ,"hiperbolic"]/base2[x-xmin ,"hiperbolic"]} else if(sex == "F"){
        kpxs <- base2[(x+n+m)-xmin ,"hiperbolicy"]/base2[x-xmin ,"hiperbolicy"]}

    sobreprima <-  (1+i)^(-n-m) * kpxs * B
    dotal <- totprim + sobreprima
    return(dotal)

  }

}

qComposicionCartera <- function(baseValuaciones){
source("FiltroAntiguedad.R")
datos2 <- subset(baseValuaciones[complete.cases(baseValuaciones),],FechaAutorizacionValuacion < "2021-01-01")
datos2[which(datos2$Modelo > 2021),"Modelo"] <- c(2019,2012,2016,2005,2018)
# Getting the valuations per brand and where the vehicle was sent to (AGENCIA/TALLER)----------------------------------------

# Counting valuations per vehicle brand
resumenGral <- as.data.frame(table(unlist(datos2$MarcaVehiculoNormalizado)))
resumenGral <- resumenGral[order(resumenGral$Freq,decreasing = TRUE),]# Ordering the results
names(resumenGral) <- c("MARCA","UNIDADES")#Renaming columns

# Selecting the TOP 25 of vehicles brand
marcasTop <- resumenGral$UNIDADES[1:25]
names(marcasTop) <- as.character(resumenGral$MARCA[1:25])#Renaming columns

# Joining the remaining brands in a unique category
marcasOtras <- sum(resumenGral$UNIDADES[26:length(resumenGral$UNIDADES)])
names(marcasOtras) <- "OTRAS"
namesOtras <- as.character(resumenGral$MARCA[26:length(resumenGral$UNIDADES)])

# Joining non top brands as OTRAS in datos2$MarcaVehiculoNormalizado
levels(datos2$MarcaVehiculoNormalizado) <- c(levels(datos2$MarcaVehiculoNormalizado),"OTRAS")
for (i in 1:length(datos2$MarcaVehiculoNormalizado)){
  if(!(datos2$MarcaVehiculoNormalizado[i] %in% names(marcasTop))){
    datos2$MarcaVehiculoNormalizado[i] <- "OTRAS"
  }
}
datos2 <- droplevels(datos2)#Removing unused factors

# Rebuilding resumenGral variable
resumenGral <- c(marcasTop,marcasOtras)
totalValuaciones <- sum(resumenGral)#Total valuations carried out within the time lapse under analysis

# Gathering information about the kind of workshop the vehicle was sent to be fixed by MarcaVehiculoNormalizado and Modelo----------------------------------------

infoagencia <- data.frame()#Data frame to collect AGENCIA information of marcasTop
infotaller <- data.frame()#Data frame to collect TALLER information of marcasTop
antiguedadMarca <- data.frame()#Data frame to collect information of vehicle year
rangoModelo <- c(as.integer(format(Sys.Date(),"%Y")),as.integer(format(Sys.Date(),"%Y"))-c(2,4,6,7))#Time intervals to group different vehicle models

for (i in names(resumenGral)){
  infoagencia <- rbind(infoagencia,subset(datos2,TipoCentroReparacion == "AGENCIA" & MarcaVehiculoNormalizado == i))#Vehicles sen to AGENCIA
  infotaller <- rbind(infotaller,subset(datos2, (TipoCentroReparacion == "TALLER" | TipoCentroReparacion == "AGENCIA MULTIMARCA") & MarcaVehiculoNormalizado == i))# Vehicles sent to TALLER/AGENCIA-MULTIMARCA
  
  for(j in 1:length(rangoModelo)){
    
    if(j == 1){
      antiguedadMarca[i,j] <- dim(subset(datos2, MarcaVehiculoNormalizado == i & Modelo == rangoModelo[j]))[1]#Last year vehicles
    }
    else if(j == length(rangoModelo)){
      antiguedadMarca[i,j] <- dim(subset(datos2, MarcaVehiculoNormalizado == i & Modelo <= rangoModelo[j]))[1]#Vehicles between the previous year up to -6 years
    }
    else{
      antiguedadMarca[i,j] <- dim(subset(datos2, MarcaVehiculoNormalizado == i & Modelo < rangoModelo[j-1] & Modelo >= rangoModelo[j]))[1]#Vehicles +7 years
    }
  }
}

antiguedadMarca <- antiguedadMarca[order(row.names(antiguedadMarca)),]#Sorting antiguedadMarca by MarcaVehiculoNormalizado

# Building data frame with information about TipoCentroReparacion and Modelo (year of the vehicle)
centroDeReparacionMarca <- cbind(as.data.frame(table(unlist(infoagencia$MarcaVehiculoNormalizado)))$Freq, as.data.frame(table(unlist(infotaller$MarcaVehiculoNormalizado)))$Freq,antiguedadMarca)
colnames(centroDeReparacionMarca) <- c("AGENCIA","TALLER",as.character(rangoModelo[1]),paste(as.character(rangoModelo[1]-1),"-",as.character(rangoModelo[2]), sep = ""),paste(as.character(rangoModelo[2]-1),"-",as.character(rangoModelo[3]), sep = ""),paste(as.character(rangoModelo[3]-1),"-",as.character(rangoModelo[4]), sep = ""),paste("<=",as.character(rangoModelo[5]), sep = ""))#Renaming columns

# Gathering information about ValorComercial and the year of the vehicle

valorVehiculo <- data.frame()
valorAgencia <- data.frame()
valorTaller <- data.frame()
rangoValorComercial <- seq(100e3,500e3,length = 5)#Value range to group different vehicle values
  
for (i in 1:length(rangoValorComercial)){
  
    if(i == 1){
      tmp <- subset(datos2, ValorComercial <= rangoValorComercial[i])#Subsetting to get vehicles with ValorComercial <= 100,000
      tmpAgencia <- subset(tmp, ValorComercial <= rangoValorComercial[i] & TipoCentroReparacion == "AGENCIA")#Subsetting tmp to get the vehicles with ValorComercial <= 100000 sent to AGENCIA
      tmpTaller <- subset(tmp, ValorComercial <= rangoValorComercial[i] & (TipoCentroReparacion == "TALLER" | TipoCentroReparacion == "AGENCIA MULTIMARCA"))#Subsetting tmp to get the vehicles with ValorComercial <= 100000 sent to TALLER/AGENCIA MULTIMARCA
      valorVehiculo <- rbind(valorVehiculo,unlist(lapply(Antiguedad(tmp, rangoModelo), dim))[c(seq(1,9,2))])
      valorAgencia <- rbind(valorAgencia,dim(tmpAgencia)[1]) 
      valorTaller <- rbind(valorTaller,dim(tmpTaller)[1]) 
      nombresRangoValor <- as.character(paste("<=",rangoValorComercial[i]/1e3, "k",sep =""))
    }
    
    else if(i == length(rangoValorComercial)){
      tmp <- subset(datos2, ValorComercial > rangoValorComercial[i-1] & ValorComercial <= rangoValorComercial[i])
      tmp2 <- subset(datos2, ValorComercial > rangoValorComercial[i])#Subsetting to get vehicles with ValorComercial > 500,000
      tmpAgencia <- subset(tmp, ValorComercial <= rangoValorComercial[i] & TipoCentroReparacion == "AGENCIA")
      tmpTaller <- subset(tmp, ValorComercial <= rangoValorComercial[i] & (TipoCentroReparacion == "TALLER" | TipoCentroReparacion == "AGENCIA MULTIMARCA"))
      valorVehiculo <- rbind(valorVehiculo,unlist(lapply(Antiguedad(tmp, rangoModelo), dim))[c(seq(1,9,2))])
      valorVehiculo <- rbind(valorVehiculo,unlist(lapply(Antiguedad(tmp2, rangoModelo), dim))[c(seq(1,9,2))])
      valorAgencia <- rbind(valorAgencia,dim(tmpAgencia)[1],dim(subset(tmp2,ValorComercial > rangoValorComercial[i] & TipoCentroReparacion == "AGENCIA"))[1]) 
      valorTaller <- rbind(valorTaller,dim(tmpTaller)[1],dim(subset(tmp2, ValorComercial > rangoValorComercial[i] & (TipoCentroReparacion == "TALLER" | TipoCentroReparacion == "AGENCIA MULTIMARCA")))[1]) 
      nombresRangoValor[i] <- as.character(paste(">",rangoValorComercial[i-1]/1e3,"k","<=",rangoValorComercial[i]/1e3,"k",sep=""))
      nombresRangoValor[i+1]<-as.character(paste(">",rangoValorComercial[i]/1e3,"k",sep=""))
    }
    else{
      tmp <- subset(datos2, ValorComercial > rangoValorComercial[i-1] & ValorComercial <= rangoValorComercial[i])
      tmpAgencia <- subset(tmp, ValorComercial <= rangoValorComercial[i] & TipoCentroReparacion == "AGENCIA")
      tmpTaller <- subset(tmp, ValorComercial <= rangoValorComercial[i] & (TipoCentroReparacion == "TALLER" | TipoCentroReparacion == "AGENCIA MULTIMARCA"))
      valorVehiculo <- rbind(valorVehiculo,unlist(lapply(Antiguedad(tmp, rangoModelo), dim))[c(seq(1,9,2))])
      valorAgencia <- rbind(valorAgencia,dim(tmpAgencia)[1]) 
      valorTaller <- rbind(valorTaller,dim(tmpTaller)[1]) 
      nombresRangoValor[i] <- as.character(paste(">",rangoValorComercial[i-1]/1e3,"k","<=",rangoValorComercial[i]/1e3,"k",sep=""))
    }
}

row.names(valorVehiculo) <- nombresRangoValor
colnames(valorAgencia) <- "AGENCIA"
colnames(valorTaller) <- "TALLER"
colnames(valorVehiculo) <- c(as.character(rangoModelo[1]),paste(as.character(rangoModelo[1]-1),"-",as.character(rangoModelo[2]), sep = ""),paste(as.character(rangoModelo[2]-1),"-",as.character(rangoModelo[3]), sep = ""),paste(as.character(rangoModelo[3]-1),"-",as.character(rangoModelo[4]), sep = ""),paste("<=",as.character(rangoModelo[5]), sep = ""))
centroReparacionValor <- cbind(valorAgencia,valorTaller,valorVehiculo)
return(list(centroDeReparacionMarca,centroReparacionValor,totalValuaciones))
}
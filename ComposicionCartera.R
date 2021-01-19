# ProporcionesPorMarca <- function(baseValuaciones){
datos2 <- subset(baseValuaciones,FechaAutorizacionValuacion < "2021-01-01")
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

# Collecting information by MarcaVehiculoNormalizado----------------------------------------

#  Gathering information about the kind of workshop the vehicule was sent to: AGENCIA/TALLER-AGENCIA MULTI-MARCA
infoagencia <- data.frame()#Data frame to collect AGENCIA information of marcasTop
infotaller <- data.frame()#Data frame to collect TALLER information of marcasTop
antiguedadMarca <- data.frame()#Data frame to collect information of vehicle year
rangoModelo <- c(as.integer(format(Sys.Date(),"%Y")),as.integer(format(Sys.Date(),"%Y"))-c(2,4,6,7))#Time intervals to group different vehicle models

for (i in names(resumenGral)){
  infoagencia <- rbind(infoagencia,subset(datos2,TipoCentroReparacion == "AGENCIA" & MarcaVehiculoNormalizado == i))
  infotaller <- rbind(infotaller,subset(datos2, (TipoCentroReparacion == "TALLER" | TipoCentroReparacion == "AGENCIA MULTIMARCA") & MarcaVehiculoNormalizado == i))
  
  for(j in 1:length(rangoModelo)){
    
    if(j == 1){
      antiguedadMarca[i,j] <- dim(subset(datos2, MarcaVehiculoNormalizado == i & Modelo == rangoModelo[j]))[1]
    }
    else if(j == length(rangoModelo)){
      antiguedadMarca[i,j] <- dim(subset(datos2, MarcaVehiculoNormalizado == i & Modelo <= rangoModelo[j]))[1]
    }
    else{
      antiguedadMarca[i,j] <- dim(subset(datos2, MarcaVehiculoNormalizado == i & Modelo < rangoModelo[j-1] & Modelo >= rangoModelo[j]))[1]
    }
  }
}

antiguedadMarca <- antiguedadMarca[order(row.names(antiguedadMarca)),]
antiguedadMarca[is.na(antiguedadMarca)] <- 0
# Building data frame with information about TipoCentroReparacion and Modelo
centroDeReparacionMarca <- cbind(as.data.frame(table(unlist(infoagencia$MarcaVehiculoNormalizado)))$Freq, as.data.frame(table(unlist(infotaller$MarcaVehiculoNormalizado)))$Freq,antiguedadMarca)
colnames(centroDeReparacionMarca) <- c("AGENCIA","TALLER",as.character(rangoModelo[1]),paste(as.character(rangoModelo[1]-1),"-",as.character(rangoModelo[2]), sep = ""),paste(as.character(rangoModelo[2]-1),"-",as.character(rangoModelo[3]), sep = ""),paste(as.character(rangoModelo[3]-1),"-",as.character(rangoModelo[4]), sep = ""),paste("<=",as.character(rangoModelo[5]), sep = ""))#Renaming columns

# Gathering information about the year of the vehicle

rangoValorComercial <- seq(100e3,500e3,length = 5)#Value range to group different vehicle values
valorVehiculo <- matrix(0,ncol = length(rangoModelo), nrow = length(rangoValorComercial)+1, byrow = TRUE)

  for (i in 1:length(rangoModelo)){
  
    if(i == 1){
    # tmp <- subset(datos2,Modelo == rangoModelo[i])
    antiguedadMarca <- as.data.frame(table(unlist(datos2$MarcaVehiculoNormalizado)))#Data frame to save the information of vehicle antiquity per brand
    
      for (j in 1:length(rangoValorComercial)){
        valorVehiculo[i,j] <- sum(table(unlist(subset(datos2,ValorComercial <= rangoValorComercial[j] & Modelo == rangoModelo[i])$MarcaVehiculoNormalizado)))
      }
      nombresRangoValor <- as.character(paste("<=",rangoValorComercial[i]/1e3, "k",sep =""))
      remove(tmp)
    }
  
    else if (i == length(rangoModelo)){
    antiguedadMarca <- cbind(antiguedadMarca,as.data.frame(table(unlist(subset(datos2,Modelo <= rangoModelo[i])$MarcaVehiculoNormalizado)))$Freq)
    valorVehiculo[i,j] <- sum(table(unlist(subset(datos2,ValorComercial > rangoValorComercial[i-1] & ValorComercial <= rangoValorComercial[i])$MarcaVehiculoNormalizado)))
    valorVehiculo[i+1,j+1] <- sum(table(unlist(subset(datos2,ValorComercial >  rangoValorComercial[i])$MarcaVehiculoNormalizado)))
    }
  
    else{
    tmp <- subset(datos2,Modelo < rangoModelo[i-1] & Modelo >= rangoModelo[i])
    antiguedadMarca <- cbind(antiguedadMarca,as.data.frame(table(unlist(tmp$MarcaVehiculoNormalizado))))
    
      for(j in 1:length(rangoValorComercial)){
        valorVehiculo[i,j] <- sum(table(unlist(subset(datos2,ValorComercial <= rangoValorComercial[j] & Modelo == rangoModelo[i])$MarcaVehiculoNormalizado)))
      }
      nombresRangoValor <- cbind(as.character(paste("")))
    }
  }

proporcionesMarcas <- cbind(centroDeReparacionMarca,antiguedadMarca[,2:dim(antiguedadMarca)[2]])
names(valorVehiculo) <- c("<=100k",">100k<=200k",">200k<=300k",">300k<=400k",">400<=500",">500k")
proporcionesValorComercial <- data.frame(valorVehiculo)
# colnames(proporcionesValorComercial) <- c("ValorComercial","Unidades")
# a<- datos2[which(datos2$Modelo == actualYr),]
# Collecting information by ValorComercial----------------------------------------

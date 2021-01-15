Historico <- function(baseValuaciones){

# Importing necessary libraries
library(tidyverse)
library(lubridate)
library(data.table)

# Verifying that column type are correct-----------------------------------------------------------------------------
# Firstly it is necessary to set FechaOcurrido and FechaAutorizacionValuacion to date because they have innecesary time stamps
baseValuaciones$FechaOcurrido <- as.Date(baseValuaciones$FechaOcurrido)#,format="%d-%m-%Y")#Converting to date
baseValuaciones$FechaAutorizacionValuacion <- as.Date(baseValuaciones$FechaAutorizacionValuacion)#,format="%d-%m-%Y")#Converting to date

actualColumnsType <- unlist(lapply(baseValuaciones,class))#Getting the column type that was read from csv
mustColumnsType <- c("factor","factor","factor","Date","Date","numeric","numeric","numeric","numeric","integer","factor","factor","integer","factor")#Required column type

idx <- actualColumnsType == mustColumnsType#Which columns don't match the correct type

names(idx[idx == FALSE])

# Factors
baseValuaciones$ValLst_NumeroExpediente <- as.factor(baseValuaciones$ValLst_NumeroExpediente)
baseValuaciones$Asegurado_Tercero <- as.factor(baseValuaciones$Asegurado_Tercero)
baseValuaciones$IdOficinaAtencion <- as.factor(baseValuaciones$IdOficinaAtencion)
baseValuaciones$MarcaVehiculoNormalizado <- as.factor(baseValuaciones$MarcaVehiculoNormalizado)
baseValuaciones$SubMarcaVehiculo <- as.factor(baseValuaciones$SubMarcaVehiculo)
baseValuaciones$TipoCentroReparacion <- as.factor(baseValuaciones$TipoCentroReparacion)

# Ordering data by date. FechaAutorizacionValuacion
baseValuaciones <- dplyr::arrange(baseValuaciones,FechaAutorizacionValuacion)
rangoFechas <- seq(min(baseValuaciones$FechaAutorizacionValuacion),by ="month",length.out = 12)

# Generating historic data-----------------------------------------------------------------------------
datosMensuales = list()
for (i in 1:length(rangoFechas)){
  if(i < length(rangoFechas)){
  datosMensuales[[i]] = subset(baseValuaciones,FechaAutorizacionValuacion >= rangoFechas[i] & FechaAutorizacionValuacion < rangoFechas[i+1])
                                }
  else{
  datosMensuales[[i]] = subset(baseValuaciones,FechaAutorizacionValuacion >= rangoFechas[i] & FechaAutorizacionValuacion < rangoFechas[i]+months(1)-days(1))
      }
}

# Getting the number of valuations, mean pieces per valuation, mean cost per piece and total replacement expenses per month
valuationsPerMonth <- sapply(datosMensuales, dim)[1,]#Counting the number of valuations per month
meanPiecesPerValuation = numeric()#declaring empty numeric array for the average of replacements per valuation
meanCostPerPiece = numeric()#declaring empty numeric array for mean cost per replacement

for (i in 1:length(valuationsPerMonth)){
  meanPiecesPerValuation[i] <- mean(datosMensuales[[i]]$NumeroTotalPiezas)#Mean pieces per valuation
  meanCostPerPiece[i] <-sum(datosMensuales[[i]]$MontoRefacciones)/sum(datosMensuales[[i]]$NumeroTotalPiezas)#Mean cost per piece
                                   }
totalReplacementExpenses <- valuationsPerMonth*meanCostPerPiece*meanPiecesPerValuation

# Generating data frame to feed the model
historico <- data.frame(rangoFechas,valuationsPerMonth,meanCostPerPiece,meanPiecesPerValuation,totalReplacementExpenses)
names(historico) <- c("Fecha","NumeroDeValuaciones","CostoPromedioPorPieza","NumeroPiezasPorValuacion","GastoTotalRefacciones")
return(list(baseValuaciones,historico))
}
# Function to check wheter a required package is loaded or not, if not, then is loaded#################################################
necessaryPkg <- function(packages){
  for (i in packages){
    if(!(i %in% .packages())){
      library(i,character.only = TRUE)
    }
  }
}

# Function to take a data frame with valuations from a long period of time and generate historical data up to the last complete month##
Historico <- function(baseValuaciones){
  
  # Importing necessary libraries
  necessaryPkg(c("lubridate","data.table","tidyverse"))
  
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
  baseValuaciones$Modelo <- as.integer(baseValuaciones$Modelo)
  baseValuaciones$TipoCentroReparacion <- as.factor(baseValuaciones$TipoCentroReparacion)
  
  # Ordering data by date. FechaAutorizacionValuacion
  baseValuaciones <- dplyr::arrange(baseValuaciones,FechaAutorizacionValuacion)
  rangoFechas <- seq(min(baseValuaciones$FechaAutorizacionValuacion),by ="month",length.out = elapsed_months(max(baseValuaciones$FechaAutorizacionValuacion),min(baseValuaciones$FechaAutorizacionValuacion)))
  
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

# This function calculates the number of months elapsed between two dates
elapsed_months <- function(end_date,start_date){
  12*(year(end_date) - year(start_date)) + (month(end_date) -month(start_date))
}
# This function takes a data frame with vehicles Year and finds the five time intervals to split them##################################
rango_modelos <- function(baseDatos){
  rangoModelo <- c(as.integer(format(Sys.Date(),"%Y"))
                   , as.integer(format(Sys.Date(),"%Y"))-c(2,4,6,7))#Time intervals to group different vehicle models
  names(rangoModelo) <- c(as.character(rangoModelo[1])
                          , paste(as.character(rangoModelo[1]-1),"-",as.character(rangoModelo[2]), sep = "")
                          , paste(as.character(rangoModelo[2]-1),"-",as.character(rangoModelo[3]), sep = "")
                          , paste(as.character(rangoModelo[3]-1),"-",as.character(rangoModelo[4]), sep = "")
                          , paste("<=",as.character(rangoModelo[5]), sep = ""))
}

# Function to clean up a data frame removing those rows with values that make no sense#################################################
basic_clean <- function(baseDatos){
  library(dplyr)
  baseDatos %>%
    filter(ValorComercial > 0 & NumeroTotalPiezas > 0 &  MontoRefacciones >= 0 & MontoTotalValuacion > 0 & Modelo <= as.integer(format(Sys.Date(),"%Y")) & Modelo >= 1886)
}

# This function separates a dataframe by vehicle's Modelo generating ##################################################################
Antiguedad <- function(tmp,rango){
  
  antig = list()#List to save dataframes of vehicles by Modelo
  for (j in 1:length(rango)){#For loop to iterate over the vehicle Model
    
    if(j == 1){
      antig[[j]] <- subset(tmp, Modelo == rango[j])#Last year model
    }
    
    else if(j == length(rango)){
      antig[[j]] <- subset(tmp, Modelo <= rango[j])
    }
    
    else{
      antig[[j]] <- subset(tmp, Modelo < rango[j-1] & Modelo >= rango[j])
    }
    
  }
  return(antig)
}

# Function to check performance of ARIMA model#########################################################################################

farima <- function(x, h) {
  forecast(auto.arima(x, stepwise = FALSE), h=h)
}

# Function to check performance of ETS model##########################################################################################
fets <- function(x, h){
  forecast(ets(x), h = h)
}

# Function to check performance of Holt-Winters model#################################################################################
fhw <- function(x, h){
  forecast(hw(x), h = h)
}

# Function to perform forecast of the best model (based on MSE calculated in the main script)#########################################
bestModel <- function(MSE,serie){
  best = names(which(MSE == min(MSE)))
  if(best == "ETS"){
    fc <- forecast(ets(serie), h = 6)
  } else if(best == "ARIMA"){
    fc <- forecast(auto.arima(serie, stepwise = FALSE), h = 6)
  } else if (best == "Holt-Winters"){
    fc <- forecast(hw(serie), h = 6)
  } else{
    print("This function only can test three models: ETS, ARIMA and Holt-Winters")
  }
}

# Funcion to take a data frame with valuations from all the company and split it by valuations per office#############################
separarOficinas <- function(file, baseDatos){
  oficinas <- read.csvoficinas <- read.csv(file, stringsAsFactors = FALSE, colClasses = c("NULL","factor", "factor", "character"))
  
  listaOficina = list()# 
# Grouping data by IdOficinaAtencion----------------------------
  agrupados  <- baseDatos %>% group_split(IdOficinaAtencion)
# Getting rid of the data belonging to offices with just a few sinisters per year----------------------------
  for(i in seq(1,length(agrupados))){
    if(dim(agrupados[[i]])[1] >= 300){
      listaOficina = list.append(listaOficina,agrupados[[i]] %>% as.data.frame())
                                    }
                                   }
  return(listaOficina)
}
# Funcion to generate forecasting for replacements, valuations and replacement's cost)################################################
predictReplacements <- function(historico, meses){
  
  necessaryPkg(c("forecast","ggplot2","MASS","tidyverse","ggpubr","broom","AICcmodavg"))
  
  tsValuaciones <- ts(historico$NumeroDeValuaciones
                      , frequency = 12
                      , start = c(as.integer(format(historico[1,1],"%Y")),as.integer(format(historico[1,1],"%m"))))
  tsCostoPieza <- ts(historico$CostoPromedioPorPieza
                     , frequency = 12
                     , start = c(as.integer(format(historico[1,1],"%Y")),as.integer(format(historico[1,1],"%m"))))
  tsPiezas <- ts(historico$NumeroPiezasPorValuacion
                 , frequency = 12
                 , start = c(as.integer(format(historico[1,1],"%Y")),as.integer(format(historico[1,1],"%m"))))
  
  # Predicting mean cost per replacement---------------------------------------------------------------------
  
  # Time Series Cross-Validation to check performance of different models
  error1costo <- tsCV(tsCostoPieza, fets, h = meses)
  error2costo <- tsCV(tsCostoPieza, farima, h = meses)
  error3costo <- tsCV(tsCostoPieza, fhw, h = meses)
  
  # Calculating the Mean Square Error for each model to pick the better
  MSEcosto <- c(mean(error1costo^2, na.rm = TRUE),mean(error2costo^2, na.rm = TRUE),mean(error3costo^2, na.rm = TRUE))
  names(MSEcosto) <- c("ETS","ARIMA","Holt-Winters")
  
  fcCosto <- bestModel(MSEcosto, tsCostoPieza)# Selecting the best model and generating forecast
  
  # Predicting replacements per valuation---------------------------------------------------------------------
  
  # Time Series Cross-Validation to check performance of different models
  error1Piezas <- tsCV(tsPiezas, fets, h = meses)
  error2Piezas <- tsCV(tsPiezas, farima, h = meses)
  error3Piezas <- tsCV(tsPiezas, fhw, h = meses)
  
  # Calculating the Mean Square Error for each model to pick the better
  MSEPiezas <- c(mean(error1Piezas^2, na.rm = TRUE),mean(error2Piezas^2, na.rm = TRUE),mean(error3Piezas^2, na.rm = TRUE))
  names(MSEPiezas) <- c("ETS","ARIMA","Holt-Winters")
  
  fcPiezas <- bestModel(MSEPiezas, tsPiezas)# Selecting the best model and generating forecast
  
  # Predicting the number of valuations---------------------------------------------------------------------
  
  modeloVal <- loess(historico$NumeroDeValuaciones~c(1:dim(historico)[1]),span=0.2)
  fcVal <- predict(modeloVal, se = TRUE)# Generating the predictions
  
  return(list(fcCosto, fcPiezas, fcVal))
}
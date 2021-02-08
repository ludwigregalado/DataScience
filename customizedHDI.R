# Function to check wheter a required package is loaded or not
necessaryPkg <- function(packages){
  for (i in packages){
    if(!(i %in% .packages())){
      library(i,character.only = TRUE)
    }
  }
}

# This function calculates the number of months elapsed between two dates
elapsed_months <- function(end_date,start_date){
  12*(year(end_date) - year(start_date)) + (month(end_date) -month(start_date))
}
# This function takes a data frame with vehicles Year and finds the five time intervals to split them
rango_modelos <- function(baseDatos){
  rangoModelo <- c(as.integer(format(Sys.Date(),"%Y"))
                   , as.integer(format(Sys.Date(),"%Y"))-c(2,4,6,7))#Time intervals to group different vehicle models
  names(rangoModelo) <- c(as.character(rangoModelo[1])
                          , paste(as.character(rangoModelo[1]-1),"-",as.character(rangoModelo[2]), sep = "")
                          , paste(as.character(rangoModelo[2]-1),"-",as.character(rangoModelo[3]), sep = "")
                          , paste(as.character(rangoModelo[3]-1),"-",as.character(rangoModelo[4]), sep = "")
                          , paste("<=",as.character(rangoModelo[5]), sep = ""))
}
# This function eliminates all valuations belonging to cars with ValorComercial <= 0, Modelo < 1837 & Modelo > 2021, 
# MontoRefacciones <= 0

basic_clean <- function(baseDatos){
  library(dplyr)
  baseDatos %>%
    filter(ValorComercial > 0 & NumeroTotalPiezas > 0 &  MontoRefacciones >= 0 & MontoTotalValuacion > 0 & Modelo <= as.integer(format(Sys.Date(),"%Y")) & Modelo >= 1886)
}

# This function separates a dataframe by vehicle's Modelo generating 

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

# Function to check performance of ARIMA model

farima <- function(x, h) {
  forecast(auto.arima(x, stepwise = FALSE), h=h)
}

# Function to check performance of ETS model
fets <- function(x, h){
  forecast(ets(x), h = h)
}

fhw <- function(x, h){
  forecast(hw(x), h = h)
}

# Function to perform forecast of the best model (based on MSE calculated in the main script)
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
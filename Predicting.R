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
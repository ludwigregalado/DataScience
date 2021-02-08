source("customizedHDI.R")
necessaryPkg(c("forecast","ggplot2","MASS","tidyverse","ggpubr","broom","AICcmodavg"))

my_data <- mutate(my_data, RefaccionesPromedio = MontoRefacciones/NumeroTotalPiezas)

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
error1costo <- tsCV(tsCostoPieza, fets, h =6)
error2costo <- tsCV(tsCostoPieza, farima, h = 6)
error3costo <- tsCV(tsCostoPieza, fhw, h = 6)

# Calculating the Mean Square Error for each model to pick the better
MSEcosto <- c(mean(error1costo^2, na.rm = TRUE),mean(error2costo^2, na.rm = TRUE),mean(error3costo^2, na.rm = TRUE))
names(MSEcosto) <- c("ETS","ARIMA","Holt-Winters")

# Selecting the best model and ploting the forecast
fcCosto <- bestModel(MSEcosto, tsCostoPieza)
autoplot(fcCosto)+ggtitle(paste("Prediccion de costo medio"
                                        , format(Sys.Date(),"%b")
                                        , "-"
                                        , format(Sys.Date()+months(6),"%b")
                                        , format(Sys.Date(),"%Y")))
# Predicting replacements per valuation---------------------------------------------------------------------

# Time Series Cross-Validation to check performance of different models

error1Piezas <- tsCV(tsPiezas, fets, h = 6)
error2Piezas <- tsCV(tsPiezas, farima, h = 6)
error3Piezas <- tsCV(tsPiezas, fhw, h =6)

# Calculating the Mean Square Error for each model to pick the better
MSEPiezas <- c(mean(error1Piezas^2, na.rm = TRUE),mean(error2Piezas^2, na.rm = TRUE),mean(error3Piezas^2, na.rm = TRUE))
names(MSEPiezas) <- c("ETS","ARIMA","Holt-Winters")

# Selecting the best model and ploting the forecast
fcPiezas <- bestModel(MSEPiezas, tsPiezas)
autoplot(fcPiezas)+xlab("Fecha")+ylab("Piezas")+ggtitle(paste("Prediccion de piezas por valuaciones"
                                , format(Sys.Date(),"%b")
                                , "-"
                                , format(Sys.Date()+months(5),"%b")
                                , format(Sys.Date(),"%Y")))

# Predicting the number of valuations---------------------------------------------------------------------
# LOESS fitting
ggplot(data = historico, aes(x=Fecha, y = NumeroDeValuaciones))+geom_line(size=1.0)+geom_smooth()+ylab("Valuaciones")+ggtitle("Modelo de regresión local para valuaciones")

modeloVal <- loess(historico$NumeroDeValuaciones~c(1:37),span=0.2)
fcVal <- predict(localLoes, se = TRUE)

valPred <- cbind(seq(historico$Fecha[dim(historico)[1]-5],by = "month",length.out = 12), as.data.frame(fcVal$fit[1:6]))
names(valPred) <- c("Fecha","Valuaciones")

ggplot(data = valPred, aes(x=Fecha, y = Valuaciones) )+geom_line(size=1.0)+geom_smooth()+ylab("Valuaciones")+ggtitle("Predicción de valuaciones feb. - jul. 2021")


totalExpenses <- fcPiezas$mean*fcCosto$mean*fcVal$fit[1:6]
# fc <- hw(tsCostoPieza, h = 6, seasonal = "additive")
# trainingSet <- window(tsCostoPieza, end = c(2020,6))
# testSet<- window(tsCostoPieza, start = c(2020,7))
# fc <- hw(trainingSet, h = 7)
# accuracy(fc,testSet)
# fc2 <- farima(trainingSet, h = 7)
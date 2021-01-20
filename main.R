library(RODBC)
library(formattable)
source("Historico.R")
source("ComposicionCartera.R")
# Reading data from DB-----------------------------------------------------------------------------
DWH<-odbcDriverConnect('driver={SQL Server};server=data.hdi.com.mx;database=Procesos;trusted_connection=true')#Connecting to Data WareHouse
baseValuaciones <- sqlQuery(DWH,"
                  SELECT 
                  v.ValLst_NumeroExpediente
                  , Asegurado_Tercero =
                    CASE
                  WHEN v.NumItem <= 1 THEN 'Asegurado'
                  ELSE 'Tercero'
                  END
                  , v.IdOficinaAtencion
                  , v.FechaOcurrido
                  , v.FechaAutorizacionValuacion
                  , v.ValorComercial
                  , v.MontoRefacciones
                  , v.MontoManoObra
                  , v.MontoTotalValuacion
                  , v.NumeroTotalPiezas
                  , v.MarcaVehiculoNormalizado
                  , v.SubMarcaVehiculo
                  , v.Modelo
                  , v.TipoCentroReparacion
                  
                  FROM Tb_BI_GrlSinValuacion v
                  
                  WHERE v.FechaAutorizacionValuacion >= '2020-01-01 00:00:00'
                  AND v.IdLineaNegocio = 4
                  AND v.TipoProceso = 'EN REPARACION'
             
                  
                  SELECT TOP 10 * FROM Tb_BI_GrlSinValuacion
                  
                 ")# Querying data from DWH

datos <- Historico(baseValuaciones[complete.cases(baseValuaciones),])#Calling a function to sort data by date and generate historical summary
mixMarcas <- ComposicionCartera(baseValuaciones)

baseValuaciones <- datos[[1]]#Complete data set sorted by date
historico <- datos[[2]]#Historical data to feerd the model
reparacionMarca <- mixMarcas[[1]]
reparacionValor <- mixMarcas[[2]]
numValuaciones <- mixMarcas[[3]]

# Generating linear regression-----------------------------------------------------------------------------

modeloValuaciones <- lm(historico$NumeroDeValuaciones[7:length(historico$NumeroDeValuaciones)-1]~c(7:length(historico$NumeroDeValuaciones)-1))#Number of valuations per month
modeloPiezas <- lm(historico$NumeroPiezasPorValuacion[7:length(historico$NumeroDeValuaciones)-1]~c(7:length(historico$NumeroDeValuaciones)-1))#Number of replacements per valuation
# modeloCostoPieza <- lm(historico$CostoPromedioPorPieza[7:length(historico$NumeroDeValuaciones)-1]~c(7:12)+baseValuaciones$ValorComercial)#Mean cost per replacement

# Generating predictions-----------------------------------------------------------------------------
nuevasValuaciones <- predict(modeloValuaciones,as.data.frame(c(13:19)))
nuevasPiezas <- predict(modeloPiezas,as.data.frame(c(13:19)))
# nuevoCosto

# Generating plots-----------------------------------------------------------------------------
# #Number of valuations per month
# plot(7:length(historico$NumeroDeValuaciones)-1,historico$NumeroDeValuaciones[7:length(historico$NumeroDeValuaciones)-1],type = "b",pch=19,lwd=2,col="green",xlab = "Mes (2020)",ylab = "Numero de valuaciones")
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkgray")
# points(7:length(historico$NumeroDeValuaciones)-1,historico$NumeroDeValuaciones[7:length(historico$NumeroDeValuaciones)-1],type = "b",pch=19,lwd=2,col="darkgreen",xlab = "Mes (2020)")
# abline(modeloValuaciones,col="darkred",lwd=2,lty=2)
# legend(10,5000,legend=c("Datos","Modelo"),col=c("darkgreen","darkred"),lty = 1:2)
# 
# #Number of replacements per valuation
# plot(7:length(historico$NumeroDeValuaciones)-1,historico$NumeroPiezasPorValuacion[7:length(historico$NumeroDeValuaciones)-1],type = "b",pch=19,lwd=2,col="green",xlab = "Mes (2020)",ylab = "Numero de Piezas por valuacion")
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkgray")
# points(7:length(historico$NumeroDeValuaciones)-1,historico$NumeroPiezasPorValuacion[7:length(historico$NumeroDeValuaciones)-1],type = "b",pch=19,lwd=2,col="darkgreen",xlab = "Mes (2020)")
# abline(modeloPiezas,col="darkred",lwd=2,lty=2)
# legend(10,4.5,legend=c("Datos","Modelo"),col=c("darkgreen","darkred"),lty = 1:2)

#Mean cost per replacement
# plot(5:12,historico$CostoPromedioPorPieza[5:12],type = "b",pch=19,lwd=2,col="green",xlab = "Mes (2020)",ylab = "Costo promedio por pieza (MXN)")
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkgray")
# points(5:12,historico$CostoPromedioPorPieza[5:12],type = "b",pch=19,lwd=2,col="darkgreen",xlab = "Mes (2020)")
# abline(modeloCostoPieza,col="darkred",lwd=2,lty=2)
# legend(10,4.5,legend=c("Datos","Modelo"),col=c("darkgreen","darkred"),lty = 1:2)

library(RODBC)
library(formattable)
source("Historico.R")
source("ComposicionCartera.R")
source("customizedHDI.R")
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
                  
                  WHERE v.FechaAutorizacionValuacion >= '2018-01-01 00:00:00'
                  AND v.IdLineaNegocio = 4
                  AND v.TipoProceso = 'EN REPARACION'
             
                  
                  SELECT TOP 10 * FROM Tb_BI_GrlSinValuacion
                  
                 ")# Querying data from DWH
baseValuaciones <- basic_clean(baseValuaciones)
datos <- Historico(baseValuaciones[complete.cases(baseValuaciones),])#Calling a function to sort data by date and generate historical summary
# baseValuaciones <- subset(baseValuaciones, Modelo >= 1886 & Modelo <= 2021 & ValorComercial > 0 & MontoRefacciones >=0 & MontoManoObra > 0 & MontoTotalValuacion > 0)
baseValuaciones$Modelo <- as.factor(baseValuaciones$Modelo)
# mixMarcas <- ComposicionCartera(baseValuaciones)

baseValuaciones <- datos[[1]]#Complete data set sorted by date
historico <- datos[[2]]#Historical data to feed the model
# reparacionMarca <- mixMarcas[[1]]
# reparacionValor <- mixMarcas[[2]]
# numValuaciones <- mixMarcas[[3]]

library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(tidyverse)

gastoRefMarca <- aov(MontoRefacciones~MarcaVehiculoNormalizado,data=baseValuaciones)
gastoRefModelo <- aov(MontoRefacciones~Modelo,data=baseValuaciones)
gastoRefMarcaModelo <- aov(MontoRefacciones~MarcaVehiculoNormalizado+Modelo, data = baseValuaciones)
gastoRefMarcaIntModelo <- aov(MontoRefacciones~MarcaVehiculoNormalizado*Modelo, data = baseValuaciones)
# Generating linear regression-----------------------------------------------------------------------------

modeloValuaciones <- lm(historico$NumeroDeValuaciones[7:length(historico$NumeroDeValuaciones)]~c(7:length(historico$NumeroDeValuaciones)))#Number of valuations per month
modeloPiezas <- lm(historico$NumeroPiezasPorValuacion~c(1:length(historico$NumeroDeValuaciones)))#Number of replacements per valuation
modeloCosto <- lm(historico$CostoPromedioPorPieza[7:length(historico$NumeroDeValuaciones)]~c(7:length(historico$NumeroDeValuaciones)))#Mean cost per replacement

# Generating predictions-----------------------------------------------------------------------------
nuevasValuaciones <- predict(modeloValuaciones,as.data.frame(c(13:18)))
nuevasPiezas <- predict(modeloPiezas,as.data.frame(c(13:24)))
nuevoCosto <- predict(modeloCosto,as.data.frame(c(13:18)))
GastoMarcaCentroReparacion <- nuevasValuaciones*(nuevasPiezas[1:6]*0.991)*(nuevoCosto*0.913)
GastoMarcaAntiguedad <- nuevasValuaciones*(nuevasPiezas[1:6]*0.985)*(nuevoCosto*0.910)
GastoValorComercial <- nuevasValuaciones*nuevasPiezas[1:6]*nuevoCosto

PrediccionMarcaCentroReparacion <- data.frame(nuevasValuaciones,nuevasPiezas[1:6]
                                              , nuevoCosto,GastoMarcaCentroReparacion
                                              , row.names = c("Ene-2021","Feb-2021","Mar-2021","Abr-2021","May-2021","Jun-2021"))
PrediccionMarcaAntiguedad <- data.frame(nuevasValuaciones,nuevasPiezas[1:6]
                                        , nuevoCosto,GastoMarcaAntiguedad
                                        , row.names = c("Ene-2021","Feb-2021","Mar-2021","Abr-2021","May-2021","Jun-2021"))
PrediccionValorComercial <- data.frame(nuevasValuaciones,nuevasPiezas[1:6]
                                       , nuevoCosto,GastoValorComercial
                                       , row.names = c("Ene-2021","Feb-2021","Mar-2021","Abr-2021","May-2021","Jun-2021"))
# Generating plots-----------------------------------------------------------------------------
# #Number of valuations per month
# plot(7:length(historico$NumeroDeValuaciones),historico$NumeroDeValuaciones[7:length(historico$NumeroDeValuaciones)],type = "b",pch=19,lwd=2,col="green",xlab = "Mes (2020)",ylab = "Numero de valuaciones")
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkgray")
# points(7:length(historico$NumeroDeValuaciones),historico$NumeroDeValuaciones[7:length(historico$NumeroDeValuaciones)],type = "b",pch=19,lwd=2,col="darkgreen",xlab = "Mes (2020)")
# abline(modeloValuaciones,col="darkred",lwd=2,lty=2)
# legend(10,5000,legend=c("Datos","Modelo"),col=c("darkgreen","darkred"),lty = 1:2)
# # 
# # #Number of replacements per valuation
# plot(7:length(historico$NumeroDeValuaciones),historico$NumeroPiezasPorValuacion[7:length(historico$NumeroDeValuaciones)],type = "b",pch=19,lwd=2,col="green",xlab = "Mes (2020)",ylab = "Numero de Piezas por valuacion")
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkgray")
# points(7:length(historico$NumeroDeValuaciones),historico$NumeroPiezasPorValuacion[7:length(historico$NumeroDeValuaciones)],type = "b",pch=19,lwd=2,col="darkgreen",xlab = "Mes (2020)")
# abline(modeloPiezas,col="darkred",lwd=2,lty=2)
# legend(10,4.5,legend=c("Datos","Modelo"),col=c("darkgreen","darkred"),lty = 1:2)

#Mean cost per replacement
# plot(5:12,historico$CostoPromedioPorPieza[5:12],type = "b",pch=19,lwd=2,col="green",xlab = "Mes (2020)",ylab = "Costo promedio por pieza (MXN)")
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkgray")
# points(5:12,historico$CostoPromedioPorPieza[5:12],type = "b",pch=19,lwd=2,col="darkgreen",xlab = "Mes (2020)")
# abline(modeloCostoPieza,col="darkred",lwd=2,lty=2)
# legend(10,4.5,legend=c("Datos","Modelo"),col=c("darkgreen","darkred"),lty = 1:2)
# ggplot(data=historico,aes(x=Fecha,y=NumeroDeValuaciones,group=1))+geom_line(linetype="dashed",color="chartreuse3",size=1)+geom_point(color="chartreuse3",size=3)+ggtitle("Valuaciones de enero 2019 a diciembre 2020")+ylab("Numero de valuaciones")+theme_dark()
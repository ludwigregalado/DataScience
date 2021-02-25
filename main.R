library(RODBC)
library(formattable)
source("Historico.R")
source("ComposicionCartera.R")
source("customizedHDI.R")
source("Predicting.R")
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
             
                 ")# Querying data from DWH

baseValuaciones <- basic_clean(baseValuaciones[complete.cases(baseValuaciones),])# Performing a basic cleaning to the data
datos <- Historico(baseValuaciones)#Calling a function to sort data by date and generate historical summary
baseValuaciones$Modelo <- as.factor(baseValuaciones$Modelo)

baseValuaciones <- datos[[1]]#Complete data set sorted by date
historico <- datos[[2]]#Historical data to feed the model

datosPorOficina <- separarOficinas("OficinasR.csv", baseValuaciones)
oficina7 = as.data.frame(datosPorOficina[[7]])
variablesPred <- predictReplacements(Historico(oficina7)[[2]], meses=6)
fcCosto <- variablesPred[[1]]
fcPiezas<- variablesPred[[2]]
fcVal <- variablesPred[[3]]

autoplot(fcCosto)+ggtitle(paste("Prediccion de costo medio"
                                , format(Sys.Date(),"%b")
                                , "-"
                                , format(Sys.Date()+months(6),"%b")
                                , format(Sys.Date(),"%Y")))

autoplot(fcPiezas)+xlab("Fecha")+ylab("Piezas")+ggtitle(paste("Prediccion de piezas por valuaciones"
                                                              , format(Sys.Date(),"%b")
                                                              , "-"
                                                              , format(Sys.Date()+months(5),"%b")
                                                              , format(Sys.Date(),"%Y")))
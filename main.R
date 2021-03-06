source("customizedHDI.R")
necessaryPkg(c('RODBC', 'zoo', 'tidyverse','lubridate','XLConnect','tictoc'))

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
                  AND v.NombreValuador NOT IN ('LUIS RODRIGUEZ','ERICK GONZALEZ','ALFONSO CRUZ','ALEJANDRO VIGIL','FERNANDO ROMERO','DAVID SILVA BERNAL')
                 ")# Querying data from DWH

baseValuaciones <- basic_clean(baseValuaciones[complete.cases(baseValuaciones),])# Performing a basic cleaning to the data
# datos <- Historico(baseValuaciones, FALSE)#Calling a function to sort data by date and generate historical summary
baseValuaciones$Modelo <- as.factor(baseValuaciones$Modelo)

listaPorOficina <- baseValuaciones %>% 
                    separarOficinas("OficinasR.csv", .) %>% 
                      simplify() %>% first()

# oficinasTest <- c(3,9,7,14,29,22)#Leon, Morelia, CDMX, Chihuahua, Puebla-Tehuacan, Culiacan

# In this section, the forecasting is carried out (takes approximately 3 hours)---------------------------------------------
tic()# Starts a counter
datosPorOficina <- listaPorOficina %>%
  lapply(.  %>% {Historico( ., TRUE)}) %>%# TRUE if you want forecasting by office; FALSE if HDI general forecast is required
  lapply(.  %>% {predictReplacements( ., meses = elapsed_months('2022-01-01',Sys.Date())+2)})
# Forecasting for HDI data
datosHDI <- baseValuaciones %>%
  Historico(., porOficina = FALSE) %>% 
  simplify() %>% last() %>% 
  predictReplacements(., meses = elapsed_months('2022-01-01',Sys.Date())+2)

# Opening the workbook in which the data will be saved
wb <- loadWorkbook("PrediccionesCostosRefacciones_2021.xlsx")

# Extracting data from forecast objects to record them into a spreadsheet
# createSheet(wb, "Nacional_Ligero")
# writeWorksheet(wb,extraerPredicciones(datosHDI),sheet = "Nacional_Ligero")
appendWorksheet(wb,extraerPredicciones(datosHDI),sheet = "Nacional_Ligero")
# createSheet(wb, "Oficina_Ligero")
# writeWorksheet(wb,extraerPredicciones(datosPorOficina),sheet = "Oficina_Ligero")
appendWorksheet(wb,extraerPredicciones(datosPorOficina),sheet = "Oficina_Ligero")
saveWorkbook(wb)
toc()# Finishes the counter and shows up the elapsed time in seconds

# Plotting section (to check some of the forecasts)--------------------------------------------------------------------------

# simplify2array()

# autoplot(fcCosto)+ggtitle(paste("Prediccion de costo medio"
#                                 , format(Sys.Date(),"%b")
#                                 , "-"
#                                 , format(Sys.Date()+months(10),"%b")
#                                 , format(Sys.Date(),"%Y")))

# autoplot(fcPiezas)+xlab("Fecha")+ylab("Piezas")+ggtitle(paste("Prediccion de piezas por valuaciones"
#                                                               , format(Sys.Date(),"%b")
#                                                               , "-"
#                                                               , format(Sys.Date()+months(5),"%b")
#                                                               , format(Sys.Date(),"%Y")))

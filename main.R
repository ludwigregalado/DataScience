source("customizedHDI.R")
necessaryPkg(c('RODBC', 'zoo', 'tidyverse','lubridate','XLConnect','tictoc'))
equipoPesado <- FALSE
cotizador <- TRUE
# Reading data from DB-----------------------------------------------------------------------------
DWH <- odbcDriverConnect(readLines("connection_R.sql"))#Connecting to Data WareHouse
baseValuaciones <- sqlQuery(DWH, paste("
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
                  
                  FROM Tb_BI_GrlSinValuacion v",
                  
                ifelse(cotizador, 
		            "WHERE v.ValLst_NumeroExpediente IN 
                (
                SELECT ValLst_NumeroExpediente
                FROM VW_bi_AutrSinValuacionRefacciones
                WHERE TipoAsignacionCotizacion = 'COTIZADOR')
		            AND v.FechaAutorizacionValuacion >= '2018-01-01 00:00:00'",
		            "WHERE v.FechaAutorizacionValuacion >= '2018-01-01 00:00:00'"),
                "AND v.IdLineaNegocio = 4
                  AND v.TipoProceso = 'EN REPARACION'
                  AND v.NombreValuador ",
		            ifelse(equipoPesado,
		            "IN","NOT IN"), "('LUIS RODRIGUEZ','ERICK GONZALEZ','ALFONSO CRUZ','ALEJANDRO VIGIL','FERNANDO ROMERO','DAVID SILVA BERNAL')"))# Querying data from DWH
close(DWH)
baseValuaciones <- basic_clean(baseValuaciones[complete.cases(baseValuaciones),])# Performing a basic cleaning to the data
# datos <- Historico(baseValuaciones, FALSE)#Calling a function to sort data by date and generate historical summary
baseValuaciones$Modelo <- as.factor(baseValuaciones$Modelo)

listaPorOficina <- baseValuaciones %>% 
                    separarOficinas("OficinasR.csv", .) %>% 
                    simplify() %>%
                    first()

# oficinasTest <- c(3,9,7,14,29,22)#Leon, Morelia, CDMX, Chihuahua, Puebla-Tehuacan, Culiacan

# In this section, the forecasting is carried out (takes approximately 3 hours)---------------------------------------------
tic()# Starts a counter
forecastPorOficina = list()
forecastNacional = list()

forecastPorOficina <- listaPorOficina %>%
  lapply(.  %>% {Historico( ., TRUE)}) %>%# TRUE if you want forecasting by office; FALSE if HDI general forecast is required
  lapply(.  %>% {predictReplacements( ., meses = elapsed_months('2022-01-01',Sys.Date()))})
# Forecasting for HDI data
forecastNacional <- baseValuaciones %>%
  Historico(., porOficina = FALSE) %>% 
  simplify() %>% last() %>% 
  predictReplacements(., meses = elapsed_months('2022-01-01',Sys.Date()))
toc()# Finishes the counter and shows up the elapsed time in seconds

if(equipoPesado){
  datosNacional <- extraerPredicciones(forecastNacional) %>% cbind(., EquipoPesado = 1)
  datosPorOficina <- extraerPredicciones(forecastPorOficina) %>% cbind(., EquipoPesado = 1)
} else{
  datosNacional <- extraerPredicciones(forecastNacional) %>% cbind(., EquipoPesado = 0)
  datosPorOficina <- extraerPredicciones(forecastPorOficina) %>% cbind(., EquipoPesado = 0)
}

if(cotizador){
  datosNacional <- datosNacional %>% cbind(., Cotizador = 1)
  datosPorOficina <- datosPorOficina %>% cbind(., Cotizador = 1)
} else{
  datosNacional <- datosNacional %>% cbind(., Cotizador = 0)
  datosPorOficina <- datosPorOficina %>% cbind(., Cotizador = 0)
}

datosDWH <- cbind(rbind(datosNacional, datosPorOficina))
datosDWH <- datosDWH %>% 
                    add_column(FechaPrediccion = Sys.Date(), .before = "Periodo")                  

datosDWH[2:8] <- lapply(datosDWH[2:8], as.integer)
datosDWH[9:13] <- lapply(datosDWH[9:13], as.numeric)
datosDWH[14:20] <- lapply(datosDWH[14:20], as.integer)

write_csv(datosDWH,"datoscOTIZADOR.csv")#, append = TRUE)
# write_csv(datosDWH,"datosDWH.csv", append = TRUE)
# 
# DWH<-DWH <- odbcDriverConnect(readLines("connection_R.sql"))#Connecting to Data WareHouse
# RODBC::sqlSave(DWH, dat = datosDWH, "PrediccionCosto_Refacciones", verbose=TRUE, fast=F, append=TRUE, rownames = FALSE)

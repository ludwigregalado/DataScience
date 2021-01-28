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
    filter(ValorComercial > 0 & NumeroTotalPiezas>0 &  MontoRefacciones > 0 & MontoTotalValuacion > 0 & Modelo <= as.integer(format(Sys.Date(),"%Y")) & Modelo >= 1886)
}
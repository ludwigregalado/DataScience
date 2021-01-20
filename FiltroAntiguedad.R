Antiguedad <- function(tmp,rangoModelo){

antig = list()#data.frame()    
  for (j in 1:length(rangoModelo)){#For loop to iterate over the vehicle Model
    
    if(j == 1){
      antig[[j]] <- subset(tmp, Modelo == rangoModelo[j])#Last year model
    }
    
    else if(j == length(rangoModelo)){
      antig[[j]] <- subset(tmp, Modelo <= rangoModelo[j])
    }
    
    else{
      antig[[j]] <- subset(tmp, Modelo < rangoModelo[j-1] & Modelo >= rangoModelo[j])
    }
    
  }
  return(antig)
}
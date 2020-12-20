#' Procesando datos
#'
#' @param datos 
#' @param config 
#'
#' @return
#' @import logging
#'
#' @examples

### Función para hacer melt() de todos los datasets ### 

meltingData <- function(datos){
  
  dataframes = c()
  
  for (dataset in seq(1, length(datos))){
    
    datos[[dataset]] <- melt(as.data.frame(datos[[dataset]]), na.rm = F)
    names(datos[[dataset]]) <- c('Country', 'Año', dataset)
    
    dataframes[[dataset]] <- datos[[dataset]]
    
  }
  
  return(dataframes)

}


###  Función para hacer melt() del target ### 

meltingTarget <- function(datos){
  
  datos <- melt(datos, na.rm = F)
  names(datos) <- c('Country', 'Año', 'Target')
  
  return(datos)
  
}

    
###  Función para hacer merge() de todos los datasets de train ### 

mergingDataTrain <- function(datos){
  
  merge <- merge(x = datos[[1]], y = datos[[2]], by = c('Country', 'Año'), all = T)
  
  for (dataset in seq(3, length(datos))) {
    
    merge <- merge(x = merge, y = datos[[dataset]], by = c('Country', 'Año'), all = T)
    
  }
  
  return(merge)
  
}

###  Función para hacer merge() del target con el merge de train ### 

# El dataset resultante es el que se usará para revisar los datos país y año que el usuario especifica
# De este modo podemos saber si ya tenemos el target y mostrarlo
# O si nos faltan features y no podemos predecir

mergingDataTotalBruto <- function(merge, target, config){
  
  merge_bruto <- merge(x = merge, y = target, by = c('Country', 'Año'), all = T)
  
  user_input <- filter(merge_bruto, Country == config$prediction$country, Año == config$prediction$year)
  user_input <- user_input[c(3:(length(names(merge_bruto))))]

  
  if(is.na(user_input[length(user_input)]) == F){
    
    logerror(paste0("El dato especificado a predecir ya existe: ", user_input[length(user_input)]),
             logger = 'log')
    
    stop()
   
  }else {
      
    for (feature in seq(1, (length(user_input) - 1))) {
      
      if(is.na(user_input[[feature]]) == T) {
        
        logerror("Error: al menos uno de los datos predictores para ese año y país no existe",
                 logger = 'log')
        stop()
    
      }

      
    }
    
    
  }
 
  
  return(merge_bruto)
  
}
  
###  Función 'Cleaned data' sin NaN ### 

CleaningData <- function(merge_bruto){
  
  dataframe <- na.omit(merge_bruto)
  
  dataframe <- dataframe[c(3:length(merge_bruto))]
  
  return(dataframe)
  
}

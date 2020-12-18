#' @title leerDatos
#'
#' @param config 
#' @param path 
#'
#' @return
#' 
#' @import data.table
#' @import logging
#'

# Leer datos de 'target'

leerDatosTarget <- function(config, path){
  
  pathDatosTarget <- paste0(path, "data/", config$data$target)
  
  
  tryCatch(expr = {
    
    datos_target <- data.table::fread(pathDatosTarget, sep = config$data$sep,
                               encoding = 'UTF-8', data.table = FALSE)
    
    
  }, error = function(e){
    
    logerror("Datos no encontrado en su ruta. Verifica el directorio de data y el config",
             logger = 'log')
    stop()
  })
  
  if(nrow(datos_target) == 0 | ncol(datos_target) == 0){
    
    logerror("Datos mal leido, verifica que tengan un buen formato. ",
             logger = 'log')
    stop()
    
  }
  
  return(datos_target)
  
}

# Datos de target leídos

######


# Leer datos de 'train'

library('XML')

# Separamos los nombres de los .csv que introduce el usuario por ',' 
# Eliminamos los espacios delante y detrás que pudiere haber con 'trimws'

tags_train <- trimws(strsplit(config$data$train, ",")[[1]])
dataframes = list()

pathDatosTrain <- function(config, path){
  
  pathDatosTrain <- paste0(path, "data/", tags_train)
  
  for (dataset in range(length(tags_train))){
    
    tryCatch(expr = {
      
      datos_train <- data.table::fread(pathDatosTrain[dataset], sep = config$data$sep,
                                        encoding = 'UTF-8', data.table = FALSE)
      
    }, error = function(e){
      
      logerror("Datos no encontrado en su ruta. Verifica el directorio de data y el config",
               logger = 'log')
      stop()
    })
    
    if(nrow(datos_train) == 0 | ncol(datos_train) == 0){
      
      logerror("Datos mal leido, verifica que tengan un buen formato. ",
               logger = 'log')
      stop()
      
    }
      
    }
  
  # Añadir dataframe a la lista vacía
  append(dataframes, datos_train, after = length(dataframes))
  dataframes.append(datos_train)
  
  return(datos_train)
  
}

leerDatosTarget(config, path)
pathDatosTrain(config, path)

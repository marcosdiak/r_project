#' Config de la aplicación
#'
#' @param path 
#'
#' @return
#' 
#' @import XML
#' @import logging
#'
#' @examples

leerConfig <- function(path){
  
  library(XML)
  
  configPath <- paste0(path, "config/config.xml")
  parsing <- xmlParse(file = configPath)
  
  tryCatch(expr = {
    
    #Leer el xml y convertirlo a lista
    config <- XML::xmlToList(xmlParse(configPath))
    
    
  }, error = function(e){
    
    logerror("Config no encontrado en su ruta. Verifica que se llame config.xml",
             logger = 'log')
    stop()
  })
  
  loginfo("Config leído correctamente", logger = 'log')
  
  
  separadoresAceptados <- config$data$sep %in% c(",", ";")
  
  if(!separadoresAceptados){
    
    logerror("'Sep' sólo puede valer ',' o ';' ", logger = 'log')
    stop()
    
  }
  
  return(config)
  
} 

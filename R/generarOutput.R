#' Title
#'
#' @param output 
#' @param config 
#' @param path 
#'
#' @import logging
#' @return
#'


library('logging')

generarOutput <- function(output, config, path){

  marcaTmp <- Sys.time()
  
  nombreArchivo <- paste0(path, "output/prediccion")
  
  tryCatch(expr = {
    
    write.csv(output$prediccion, file = nombreArchivo)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado!!", logger = 'log')
    stop()
  })
  
  
  nombreArchivo <- paste0(path, "output/modelo.rds")
  
  tryCatch(expr = {
    
    saveRDS(output$modelo, file = nombreArchivo)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado del modelo!!", logger = 'log')
    stop()
  })
  
  
}


generarOutput(output, config, path)

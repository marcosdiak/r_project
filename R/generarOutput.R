
#' Generando output
#'
#' @param output 
#' @param config 
#' @param path 
#'
#' @return
#' @export
#'
#' @examples

generarOutput <- function(output, config, path){
  
  library('logging')
  
  marcaTmp <- Sys.time()
  
  nombreArchivo <- paste0(path, "output/prediccion.csv")
  
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



#' @title asesinatos
#' @description Funcion principal del paquete de asesinatos
#' 
#' @param path, string 
#' 
#' @export
#' @import logging
#'
#' @author Zorion, Tilda, Marcos

asesinatos <- function(path){
  
  tryCatch(expr = {
    
    library(logging)
    
    #Generando el manejado de log
    addHandler(writeToFile, logger = 'log', file = paste0(path, "/log/logfile.log"))
    loginfo("... Inicializando aplicación ...", logger = 'log')
    
    loginfo("Leyendo el config...", logger = 'log')
    config <- leerConfig(path)

    
    loginfo("Leyendo los datos de target...", logger = 'log')
    target <- leerDatosTarget(config, path)
    loginfo("Datos de target leídos correctamente", logger = 'log')
    
    loginfo("Leyendo los datos de train...", logger = 'log')
    train <- leerDatosTrain(config, path)
    loginfo("Datos de train leídos correctamente", logger = 'log')
    
    
    loginfo("Procesando los datos de train...", logger = 'log')
    train <- meltingData(train)
    train <- mergingDataTrain(train)
    loginfo("Datos de train procesados correctamente", logger = 'log')
    
    loginfo("Procesando los datos de target...", logger = 'log')
    target <- meltingTarget(target)
    loginfo("Target procesado correctamente", logger = 'log')
    
  
    loginfo("Revisando viabilidad de petición de usuario...", logger = 'log')
    merge_bruto <- mergingDataTotalBruto(train, target, config)
    # Justo aquí, ahora, se realiza el chequeo de si se puede seguir adelante:
    # Revisar si para el año y país introducido por el usuario se puede hacer el modelo
    
    loginfo("Petición correcta", logger = 'log')
    dataframe <- CleaningData(merge_bruto)
  
    
    loginfo("Generando modelo...", logger = 'log')
    output <- generarModelo(dataframe, merge_bruto, config)
    loginfo("Modelo generado", logger = 'log')
    
    
    loginfo("Obteniendo output...", logger = 'log')
    generarOutput(output, config, path)
    loginfo("¡Output listo!", logger = 'log')
    loginfo("...", logger = 'log')
    
    
  }, error = function(e){
    
    logerror("La aplicacion se ha detenido...", logger = 'log')
    stop()
    
  },finally = {
    
    loginfo("La ejecución de la aplicación ha finalizado", logger = 'log')
    removeHandler(writeToFile, logger = 'log')
    
  })
  
  
}

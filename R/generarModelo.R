#' Generando modelo predictivo
#'
#' @param datos 
#' @param config 
#'
#' @import xgboost
#' @import logging
#'
#' @examples

generarModelo <- function(dataframe, merge_bruto, config){
  
  trainingRowIndex <- sample(1:nrow(dataframe), 0.8*nrow(dataframe))
  trainingData <- dataframe[trainingRowIndex, ]
  testData  <- dataframe[-trainingRowIndex, ]
  
  lmMod <- lm(Target ~ ., data=trainingData)
  
  # Predicción
  
  prediction <- filter(merge_bruto, Country == config$prediction$country, Año == config$prediction$year)
  prediction <- prediction[c(3:(length(names(merge_bruto)) - 1))]
  
  output <- predict(lmMod, prediction)

  return(list(prediccion = output, modelo = lmMod))

}

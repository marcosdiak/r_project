#' Title
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
  predictions <- predict(lmMod, testData)
  
  summary(lmMod)
  
  actuals_preds <- data.frame(cbind(actuals=testData$Target, predicteds=predictions)) 
  correlation_accuracy <- cor(actuals_preds)
  head(actuals_preds)
  
  # Alternately, you can compute all the error metrics in one go using the regr.eval() function in DMwR package. 
  # You will have to install.packages('DMwR') for this if you are using it for the first time.
  DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)
  
  # Cross-validation
  cvResults <- suppressWarnings(CVlm(data=dataframe, form.lm=Target ~ ., 
                                     m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  
  attr(cvResults, 'ms')  
  
  # Predicción
  
  prediction <- filter(merge_bruto, Country == config$prediction$country, Año == config$prediction$year)
  prediction <- prediction[c(3:(length(names(merge_bruto)) - 1))]
  
  output <- predict(lmMod, prediction)

  return(list(prediccion = output, modelo = lmMod))

}



output <- generarModelo(d, merge_bruto, config)

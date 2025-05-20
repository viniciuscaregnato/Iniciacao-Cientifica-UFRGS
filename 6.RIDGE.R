ridge <- function(df, target_var, ART, horizon, alpha = 0){
  
  require(glmnet)
  
  prep_data <- dataprep(df, target_var, ART, horizon)
  
  Xin <- as.matrix(prep_data$Xin)
  Yin <- as.matrix(prep_data$Yin)
  Xout <- as.matrix(prep_data$Xout)
  
  # RIDGE aplicado por cross validation
  modelo <- cv.glmnet(Xin,Yin, type.measure = "mse", alpha = alpha, family="gaussian")
  
  previsao <- predict(modelo, newx = Xout)
  
  return(previsao = previsao)
  
  
}

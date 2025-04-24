random_forest <- function(df, target_var, ART, horizon=1) {
  require(randomForest)
  
  prep_data <- dataprep(df, target_var, ART, horizon =1)
  
  yin  <- prep_data$Yin
  Xin  <- prep_data$Xin
  Xout <- prep_data$Xout
  df_final <- prep_data$df_final
  
  modelo <- randomForest(x = Xin, y = yin)
  
  previsao <- predict(modelo, newdata = Xout)
  
  return(previsao)
}

regressao_linear <- function(df_window, target_var, ART, horizon =1){
  
  
  prep_data <- dataprep(df_window, target_var, ART)
  
  yin  <- prep_data$Yin
  Xin  <- prep_data$Xin
  Xout <- prep_data$Xout
  df_window_final <- prep_data$df_window_final
  
  modelo <- lm(yin ~ ., data=Xin)
  
  previsao <- predict(modelo, newdata = Xout)
  
  
  resultados <-  return(list(previsao = previsao, yin = yin, Xin = Xin, Xout = Xout))
  
}
regressao_linear <- function(df, target_var, ART, horizon =1){
  
  
  prep_data <- dataprep(df, target_var, ART)
  
  yin  <- prep_data$Yin
  Xin  <- prep_data$Xin
  Xout <- prep_data$Xout
  df_final <- prep_data$df_final
  
  modelo <- lm(yin ~ ., data=Xin)
  
  previsao <- predict(modelo, newdata = Xout)
  
  
    resultados <-  return(list(previsao, df_final))
  
}

regressao_linear <- function(df_window, target_var, ART, horizon){
  
  
  prep_data <- dataprep(df_window, target_var, ART, horizon)
  
  data_in <- prep_data$data_in                      # trago data.frame data_in para poder manipulá-lo
  
  Xout <- prep_data$Xout                            # trago o Xout para aplicá-lo
  
  modelo <- lm(Yin ~ ., data=data_in)
  
  previsao <- predict(modelo, newdata = Xout)
  
  
  resultados <-  return(previsao = previsao)
  
}

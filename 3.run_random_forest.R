random_forest <- function(df, target_var, ART, horizon) {
  require(randomForest)
  
  prep_data <- dataprep(df, target_var, ART, horizon)
  
  data_in <- prep_data$data_in                      # trago data.frame data_in para poder manipulá-lo
  
  Xin <- data_in[, setdiff(names(data_in), "Yin")]   # Defino que Xin sera td q nao for Yin, em data_in
  Yin <- data_in[,1]                                # Como coloquei cbin(Yin,Xin), a primeira coluna é sempre Yin
  Xout <- prep_data$Xout
  
  modelo <- randomForest(x = Xin, y = Yin)
  
  previsao <- predict(modelo, newdata = Xout)
  
  return(previsao)
}

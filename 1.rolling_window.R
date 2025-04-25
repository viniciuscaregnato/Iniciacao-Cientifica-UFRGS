rolling_window <- function(df, target_var, ART, horizon, n_predicts = 2, fnct) {
  
  # n_predicts: numero de observações out-of-sample/numero de Xout
  
  previsoes <- vector("list", n_predicts)                              #cria uma lista vazia de tamanho n_predicts
  
  for (i in 0:(n_predicts - 1)) {
    
    
    df_window <- df[(1 + i):(nrow(df) - n_predicts + i), ]  # pega do primeiro Xin ate o ultimo Yin de cada "i+1"° janela
    
    
    resultado <- fnct(df_window, target_var, ART, horizon)
    
    
    previsoes[[i + 1]] <- resultado[[1]] 
    
    
  }
  
  
  
  
  return(previsoes)
}

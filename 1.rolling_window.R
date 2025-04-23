rolling_window <- function(df, target_var, ART, horizon = 1, n_predicts = 20, fnct) {

  # n_predicts: numero de observações Yout ou Xout
  
  previsoes <- vector("list", n_predicts)                              #cria uma lista vazia de tamanho n_predicts
  
  for (i in 0:(n_predicts - 1)) {
    
    
    df_window <- df[(1 + i):(nrow(df) - n_predicts + i + horizon), ]   # pega do primeiro Xin ate o ultimo Yin de cada "i+1"° janela
    
    
    resultado <- fnct(df_window, target_var, ART, horizon)
    
    
    previsoes[[i + 1]] <- resultado[[1]] 
    
    
  }
  
    
  
  
  return(previsoes)
}

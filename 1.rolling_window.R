rolling_window <- function(df, target_var, ART, horizon = 1, n_predicts = 20, fnct) {
  
  previsoes <- vector("list", n_predicts)                              #cria uma lista vazia de tamanho n_predicts
  
  for (i in 0:(n_predicts - 1)) {
    
    
    df_window <- df[(1 + i):(nrow(df) - n_predicts + i + horizon), ]
    
    
    resultado <- fnct(df_window, target_var, ART, horizon)
    
    
    previsoes[[i + 1]] <- resultado[[1]] 
    
    
  }
  
    
  return(previsoes)
}

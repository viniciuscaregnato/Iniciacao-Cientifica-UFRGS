dataprep <- function(df, target_var, ART, horizon=1) {
  
  # df: janela recebida de rolling window
  # target_var: variável dependente
  # ART: quantidade de defasagens (autoregressive terms)
  # horizon: horizonte de previsao
  
  #### criando colunas de defasagens separadas do resto ####
  
  y_art <- embed(df[[target_var]], ART)                   # cria df de defasagens de y
  colnames(y_art) <- paste0(target_var, "_t-", 1:ART)     # nomeia as colunas de y_t-..
  
  #### ajustando o df geral ####
  
  df_trimmed <- df[ART:nrow(df), ]                        # exclue as linhas perdidas ao aplicar ART
  
  #### criando daframe de regressores ####
  
   
  X_all <- df_trimmed[, setdiff(names(df_trimmed), target_var)]  # remove coluna target_var de df_trimmed para montar Xin/Xout
  
  #### criando dataframe completo com todas as informações ####
  
  X_full <- cbind(X_all, y_art)                           # junta defasagens com as demais variáveis independentes
  
  
  #### concluindo cada parte ####
  
  Xin  <- X_full[1:(nrow(X_full)-horizon),]
  Xout <- X_full[nrow(X_full)-horizon+1,]
  Yin  <- df_trimmed[[target_var]][(1+horizon):(nrow(Xin)+horizon)]  
  
  return(list(Xin = Xin, Yin = Yin, Xout = Xout, df_final = cbind(df_trimmed, y_art)))
}


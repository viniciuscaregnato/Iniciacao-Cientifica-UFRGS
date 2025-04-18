dataprep <- function(df, target_var, ART) {
  
  # df: janela recebida de rolling window
  # target_var: variável dependente
  # ART: quantidade de defasagens (autoregressive terms)
  
  y_art <- embed(df[[target_var]], ART)
  df_trimmed <- df[ART:nrow(df), ]
  
  # cria colunas de defasagens com nomes claros
  colnames(y_art) <- paste0(target_var, "_t-", 1:ART)
  
  # remove coluna target_var de df_trimmed para montar Xin/Xout
  X_all <- df_trimmed[, setdiff(names(df_trimmed), target_var)]
  
  # junta defasagens com as demais variáveis independentes
  X_full <- cbind(X_all, y_art)
  
  Xin  <- X_full[1:(nrow(X_full) - 1), ]
  Xout <- X_full[nrow(X_full), , drop = FALSE]
  Yin  <- df_trimmed[[target_var]][2:nrow(df_trimmed)]  # target deslocado
  
  return(list(Xin = Xin, Yin = Yin, Xout = Xout, df_final = cbind(df_trimmed, y_art)))
}

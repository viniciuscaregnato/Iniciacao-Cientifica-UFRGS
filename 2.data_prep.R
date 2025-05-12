dataprep <- function(df_window, target_var, ART, horizon) {
  
  # resumo: o dataprep pega cada window, aplica ART e organiza Xin, yin e Xout
  
  # df_window: janela recebida de rolling window
  # target_var: variável dependente
  # ART: quantidade de defasagens (autoregressive terms)
  # horizon: horizonte de previsao
  
  #criando vetor de y
  y_art <- embed(df_window[[target_var]], ART) 
  colnames(y_art) <- paste0(target_var, "_t-", 1:ART)
  
  
  # criando janela sem linhas de ART perdidos
  IN_all <- df_window[(ART+1):(nrow(y_art)+ART), ]   
  
  
  #adicionado defasagens à janela de 
  final_window <- cbind(IN_all, y_art)
  final_window <- na.omit(final_window)
  
  
  #janela de independents var
  X_only <- final_window[, setdiff(names(final_window), target_var)]
  
  
  # criando Xin
  Xin <- final_window[, setdiff(names(final_window), target_var)]
  Xin <- Xin[1:(nrow(Xin)-horizon),]
  
  
  #criando Xout
  Xout <- final_window[, setdiff(names(final_window), target_var)]
  Xout <- Xout[(nrow(Xin))+1,]
  
  
  #criando Yin
  Yin <- final_window[horizon+1:nrow(final_window),target_var]
  Yin <- na.omit(Yin)
  Yin <- as.data.frame(Yin)
  
  
  #### juntando tudo ####
  data_in <- cbind(Yin,Xin)
  
  
  
  return(list(Xin = Xin, Yin = Yin, Xout = Xout, data_in = data_in))
}

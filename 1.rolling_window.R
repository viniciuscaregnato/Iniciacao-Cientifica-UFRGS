rolling_window <- function(df, target_var, ART, horizon=1, lags=0, fnct){
  
  prep_data <- dataprep(df, target_var, ART, horizon)
  
  if( lags == 0 ){
    df_final = prep_data$df_final
  }else{
    df_final = prep_data$df_final[1:lags,]
  }
  

  number_window <- nrow(df) - Xin
  
  indmat <- matrix(NA, number_window,window_size)     # cada coluna representa um indice de df dentro da janela
  indmat[1,]=1:ncol(indmat)  
  
  for(i in 2:nrow(indmat)){                           # cada linha é uma janela de observação dos devidos indices
    indmat[i,]=indmat[i-1,]+1
  }
  
  rw=apply(indmat,1, fnct, df)
  
  yin  <- prep_data$Yin
  Xin  <- prep_data$Xin
  Xout <- prep_data$Xout
  df_final <- prep_data$df_final
  
}
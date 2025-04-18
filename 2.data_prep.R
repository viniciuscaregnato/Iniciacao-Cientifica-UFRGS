data_prep <- function(df, target_var, independents_var, ART=2, horizon=1) {
  
  require(dplyr)
  
  # df: janela recebida de rolling_window
  # target_var: variavel alvo
  # independents_var: variavel dependente
  # AutoRegressive Terms: numero de defasagens incluidas no modelo
  
  for (i in seq_len(ART)) {
    lag_name <- paste0(target_var, "_lag", i)
    df[[lag_name]] <- dplyr::lag(df[[target_var]], n = i)
  }
  
  
  features <- c(independents_var,
                paste0(target_var, "_lag", seq_len(ART)))
  
  df[[paste0(target_var, "_lead")]] <- dplyr::lead(df[[target_var]], n = horizon)
  
  df_clean <- df %>%
    filter(if_all(all_of(features), ~ !is.na(.x)),
           !is.na(.data[[paste0(target_var, "_lead")]]))
  
  Xin  <- as.matrix(df_clean[, features])
  yin  <- df_clean[[paste0(target_var, "_lead")]]
  Xout <- tail(Xin, n = horizon)            
  Yin_out <- tail(df_clean[[target_var]], horizon)
  
  return(list(
    Xin       = Xin,        # preditoras in-sample
    yin       = yin,        # target in-sample (lead de Y)
    Xout      = Xout,       # preditoras out-of-sample
    Yout_real = Yin_out     # valor real para comparaçao
  ))
}

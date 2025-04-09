# a rolling_window apenas calcula a matriz de janelas
# e passa para o dataprep as devidas janelas

rolling_window <- function(dados, insample = 1, horizon=1, lag = 4, n_windows=200, variable  = "CPIAUCSL"){
  
  
  
  if (insample==1){
    from <- 1
    to <- nrow(dados)
  }
  else 
  { 
    from <- 1
    to <- nrow(dados)*insample
    
  }
  
  dados_insample <- dados[from:to,]
  
  tamanho_janela <- nrow(dados_insample) - horizon - lag + 1
  first_window <- c(nrow(dados_insample)-tamanho_janela+1:nrow(dados_insample))
  
  final_matrix <- matrix(NA, nrow = n_windows, ncol = length(first_window))
  
  
  for(i in 1:n_windows){
    final_matrix[i, ] <- first_window + (i-1)
  }
  
  return(final_matrix)
}






##teste 
load("data.rda")

as.matrix(data)
matriz_teste <- rolling_window(data)
View(matriz_teste)

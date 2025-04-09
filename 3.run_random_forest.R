random_forest <- function(dados) {
  library(randomForest)
  
  dados_prontos <- teste
  
  floresta <- randomForest(dados$x, dados$y)
  
  return(floresta)
}
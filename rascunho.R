#rascunho, dados sequenciais

os_dados <- get(load("data.rda"))
View(os_dados)



data_prep <- function(dados, insample = 1, horizon=1, lag = 12, variable  = "CPIAUCSL") {
  
  #insample: tamanho da amostra insample (de 0.0 a 1.0)
  #dados: arquivo analisado
  #varible: variavel dependente
  #horizon (função rolling_window): horizonte de previsao,
  #lag (função rolling_window): defasagem
  
 if (insample==1){
     from <- 1
     to <- nrow(dados)
     }
  else 
    { 
      from <- 1
      to <- nrow(dados)*insample
    
  }
  
  data_sample <- dados[from:to,]
  
  ajuste_com_horizonte <- nrow(data_sample)-horizon-lag+1
  
 y <- data_sample[,variable]
 x <- data_sample[,setdiff(names(data_sample),variable)]

 return(list(x=x, y=y, dados_prontos=data_sample))
}


teste <- data_prep(os_dados, "INDPRO", 0.7)


random_forest <- function(dados) {
  library(randomForest)
  
  dados_prontos <- teste
  
  floresta <- randomForest(dados$x, dados$y)
  
  return(floresta)
}

modelo <- random_forest(teste)
summary(modelo)

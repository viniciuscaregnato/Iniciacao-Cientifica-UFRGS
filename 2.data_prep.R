data_prep <- function(data, variable, insample = 1) {
  
  #insample: tamanho da amostra insample (de 0.0 a 1.0)
  #dados: arquivo analisado
  #varible: variavel dependente
  
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
  y <- data_sample[,variable]
  x <- data_sample[,setdiff(names(data_sample),variable)]
  
  return(list(x=x, y=y, dados_prontos=data_sample))
}



df <- read.csv("dados_q1_l7.csv")
View(df)

#### 1° janela ####

i=0
n_predicts = 4
target_var = "y"

#### 2° janela ####

i=1
n_predicts = 2
target_var = "y"

#### 3° janela ####

i=2
n_predicts = 4
target_var = "y"

#### 4° janela ####

i=3
n_predicts = 4
target_var = "y"

#### janela dada por: ####
df_window <- df[(1 + i):(nrow(df) - n_predicts + i), ]
View(df_window)




#### data_prep ####
horizon = 3
ART = 2

#criando vetor de defasagens
y_art <- embed(df_window[[target_var]], ART) 
colnames(y_art) <- paste0(target_var, "_t-", 1:ART)
View(y_art)

# criando janela sem linhas de ART perdidos
IN_all <- df_window[(ART+1):(nrow(y_art)+ART), ]   
View(IN_all)

#adicionado defasagens à janela de 
final_window <- cbind(IN_all, y_art)
final_window <- na.omit(final_window)
View(final_window)

#janela de independents var
X_only <- final_window[, setdiff(names(final_window), target_var)]
View(X_only)

# criando Xin
Xin <- final_window[, setdiff(names(final_window), target_var)]
Xin <- Xin[1:(nrow(Xin)-horizon),]
View(Xin)

#criando Xout
Xout <- final_window[, setdiff(names(final_window), target_var)]
Xout <- Xout[(nrow(Xin))+1,]
View(Xout)

#criando Yin
Yin <- final_window[horizon+1:nrow(final_window),target_var]
Yin <- na.omit(Yin)
Yin <- as.data.frame(Yin)
View(Yin)

# juntando tudo #
data_in <- cbind(Yin,Xin)
View(data_in)

#### Aplicando a função de regressao ####
modelo <- lm(Yin ~ ., data = data_in)
previsao <- predict(modelo, newdata=Xout)

#### Aplicando a função de random forest ####
Xin <- data_in[, setdiff(names(data_in), "Yin")]
View(Yin)
Yin <- data_in[,1]
View(Yin)
modelo <- randomForest(x = Xin, y = Yin)
previsao <- predict(modelo, newdata = Xout)

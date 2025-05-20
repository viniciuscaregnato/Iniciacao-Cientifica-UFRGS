df <- read.csv("dados_q1_l7.csv")
View(df)

#### regressao linear ####
teste1 <- rolling_window(df, "y", ART=2, horizon=3, n_predicts=4, regressao_linear)

teste2 <- rolling_window(df, "y", ART=4, horizon=3, n_predicts=4, regressao_linear)

teste3 <- rolling_window(df, "y", ART=2, horizon=2, n_predicts=4, regressao_linear)

teste4 <- rolling_window(df, "y", ART=2, horizon=3, n_predicts=2, regressao_linear)

#### random forest #####
teste1 <- rolling_window(df, "y", ART=1, horizon=3, n_predicts=4, random_forest)

teste2 <- rolling_window(df, "y", ART=4, horizon=3, n_predicts=4, random_forest)

teste3 <- rolling_window(df, "y", ART=2, horizon=2, n_predicts=4, random_forest)

teste4 <- rolling_window(df, "y", ART=2, horizon=3, n_predicts=2, random_forest)

#### ridge ####

teste.ridge <- rolling_window(df, "y", ART=1, horizon=3, n_predicts=4, RIDGE)

#### lasso ####

teste.lasso <- rolling_window(df, "y", ART=1, horizon=3, n_predicts=4, LASSO)

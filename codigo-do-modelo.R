# Dataset utilizado no código disponível em: https://archive.ics.uci.edu/ml/datasets/Student+Performance

# Carregando o dataset
df <- read.csv2('estudantes.csv')

# Explorando os dados
View(df)
summary(df)
str(df)
?any
any(is.na(df)) # verificação de valores NA no dataset

library(dplyr)

# Verificando a correlação entre as colunas numéricas
library(corrplot)
col_numericas <- sapply(df, is.numeric) # extraindo as colunas numéricas
length(col_numericas)

?cor
cor(df[,col_numericas]) # correlação

corrplot(cor(df[, col_numericas]), method = 'color') # plotando a correlação

# Após a verificação, foi observado que não há nenhuma forte correlação entre as 
# variáveis numéricas.
# Chama atenção uma leve correlação positiva entre as variáveis:
#         - Dalc x Walc
#         - goout x Walc
#         - Medu x Fedu
# Chama atenção uma leve correlação negativa entre as variáveis:
#         - failures x G1, G2 e G3
#         - failures x Medu e Fedu
#         - studytime x Walc

# Analisando as variáveis:
library(ggplot2)
library(ggthemes)
library(gridExtra)
hist1 <- ggplot(df, aes(Dalc)) +
  geom_histogram(bins = 30) # Consumação de Álcool durante de trabalho

hist2 <- ggplot(df, aes(Walc)) +
  geom_histogram(bins = 30) # Consumação de Álcool no final de semana

hist3 <- ggplot(df, aes(x = goout)) +
  geom_histogram(bins = 30) # Frequências de saídas com os amigos

hist4 <- ggplot(df, aes(x = Medu)) +
  geom_histogram(bins = 30) # Escolaridade da mãe

hist5 <- ggplot(df, aes(x = Fedu)) +
  geom_histogram(bins = 30) # Escolaridade do pai

hist6 <- ggplot(df, aes(x = failures)) +
  geom_histogram(bins = 30) # Frequência de reprovações

grid.arrange(hist1, hist2, hist3, hist4, hist5, hist6)


# Analisando as variáveis G1, G2 e G3
plot1 <- ggplot(df, aes(G1)) +
  geom_histogram(bins = 20, 
                 alpha = 0.5,
                 fill = 'black') +
  theme_minimal()

plot2 <- ggplot(df, aes(G2)) +
  geom_histogram(bins = 20,
                 alpha = 0.5,
                 fill = 'yellow') +
  theme_minimal()

plot3 <- ggplot(df, aes(G3)) +
  geom_histogram(bins = 20,
                 alpha = 0.5,
                 fill = 'red') +
  theme_minimal()

grid.arrange(plot1, plot2, plot3, ncol = 1)

# Obs.: Chama atenção o número de reprovações na 2a avaliação (G2) e na avaliação final (G3)


# Criando as amostras de forma randômica
library(caTools)
amostra <- sample.split(df$age, SplitRatio = 0.70)


# Dados de treino
treino <- subset(df, amostra == T)


# Dados de teste
teste <- subset(df, amostra == F)


# Criando os modelos
modelo_1 <- lm(G3 ~ ., treino)
modelo_2 <- lm(G3 ~ G1 + G2, treino)
modelo_3 <- lm(G3 ~ absences, treino)
modelo_4 <- lm(G3 ~ Medu, treino)
modelo_5 <- lm(G3 ~ Fedu, treino)
modelo_6 <- lm(G3 ~ failures, treino)
modelo_7 <- lm(G3 ~ goout, treino)
modelo_8 <- lm(G3 ~ Walc, treino)


# Análise dos modelos
summary(modelo_1) 
summary(modelo_2) 
summary(modelo_3) 
summary(modelo_4) 
summary(modelo_5) 
summary(modelo_6) 
summary(modelo_7) 
summary(modelo_8) 


# Visualizando as taxas de erro (resíduos) do modelo escolhido
res <- residuals(modelo_1)
res <- as.data.frame(res)
res

ggplot(res, aes(res)) +
  geom_histogram(bins = 20,
                 alpha = 0.20,
                 fill = 'blue') +
  theme_minimal()


# Plot do modelo
plot(modelo_1)


# Prevendo as notas finais
previsao_G3 <- predict(modelo_1, teste)
as.data.frame(previsao_G3)


# Comparando os dados previstos com os reais
comparacao <- cbind(as.integer(previsao_G3), teste$G3)
comparacao <- as.data.frame(comparacao)
colnames(comparacao) <- c("Previsto", "Real")
View(comparacao)


# Tratando valores negativos
tratamento <- function(x){
  if (x < 0) {
    return(0)
  } else{
    return(x)
  }
}

comparacao$Previsto <- sapply(comparacao$Previsto, tratamento)
View(comparacao)


# Calculando o erro médio
# MSE:
mse <- mean((comparacao$Real - comparacao$Previsto)^2)
print(mse) # Distância dos valores previstos para os valores observados


# Calculando R Squared
SSE = sum((comparacao$Previsto - comparacao$Real)^2)
SST = sum((mean(df$G3) - comparacao$Real)^2)


# R-Squared
R2 = 1 - (SSE/SST)
R2*100 # Percentual de precisão do modelo criado

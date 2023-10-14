#Install packges 
install.packages("plotly")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("pacman")

library(plotly)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(pacman)

#Import the dataset 
Fraud = read.csv2("Fraud.csv") #Não rodar

#View the base 
dim(Fraud)

#View six lines of the base: 
head(Fraud)

#Resume informations 
names(Fraud)
summary(Fraud)

#Verify missing values
sum(is.na(Fraud))

#Verify informations in the column "ifFraud
table(Fraud$isFraud)

table(Fraud$isFlaggedFraud)

table(Fraud$type)

#How many times show off the same identify?
table(Fraud$nameOrig)
table(Fraud$nameDest)

#Plot graph using variables NameDest and NameOrig:
top_5_valoresD <- Fraud %>% 
  group_by(nameDest) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  pull(nameDest)

top_5_valoresO <- Fraud %>% 
  group_by(nameOrig) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  pull(nameOrig)

print(top_5_valoresD)
print(top_5_valoresO)

#Criando um gráfico de barras
graficoD <- barplot(contagemD, main = "Top 5 valores mais repetidos", xlab = "Valores", ylab = "Contagem") +
text(grafico, contagem, labels = contagem, pos = 3, cex = 0.8)

#Verificar o motivo de näo estar gerando o gráfico
graficoO <- barplot(contagemO, main = "Top 5 valores mais repetidos", xlab = "Valores", ylab = "Contagem") +
text(grafico, contagem, labels = contagem, pos = 3, cex = 0.8)

#About this variables, how the users is the more transactions? 
relação_usuario_transacao <- Fraud %>% 
  group_by(nameOrig, nameDest, type) %>% 
  summarise(contagem = n())

relação_usuario_transacao_tipo <- relação_usuario_transacao %>% 
  group_by(nameOrig, type) %>% 
  summarise(contagem = n())

relação_usuario_transacao_tipo2 <- relação_usuario_transacao %>% 
  group_by(nameDest, type) %>% 
  summarise(contagem = n())

#Show of the how many times about real transactions:
table(relação_usuario_transacao_tipo$contagem)
table(relação_usuario_transacao_tipo2$contagem)

#Now, count how many transactions one by one:
transações_por_identificador <- relação_usuario_transacao_tipo %>% 
  group_by(nameOrig) %>% 
  summarise(total_transacoes=sum(contagem))

transações_por_identificador2 <- relação_usuario_transacao_tipo2 %>% 
    group_by(nameDest) %>% 
    summarise(total_transacoes=sum(contagem))

#FInd the transaction with max number: 
identificador_mais_transações <- transações_por_identificador$nameOrig[which.max(transações_por_identificador$total_transacoes)]

identificador_mais_transações2 <- transações_por_identificador2$nameDest[which.max(transações_por_identificador2$total_transacoes)]

mais_transacoes <- max(transações_por_identificador$total_transacoes)

transações_por_identificador <- table(total_transacoes$nameOrig)

#Now, its more important how transactions with make with each other:
relação_id <- relação_usuario_transacao %>% 
  group_by(nameDest, nameOrig) %>% 
  summarise(total_transacoes=sum(contagem))

#Use groupby with mutate for identify relationships with nameOrig by step and nameDest by step
exemplo_mutate <- Fraud %>% 
  group_by(nameOrig) %>% 
  mutate(quant_Orig = n(), acumulado = row_number())

exemplo_mutate2 <- Fraud %>% 
  group_by(nameDest) %>% 
  mutate(quant_Dest = n())

#Identify transactions sent in the count and in the same moment leaving
cont_0_Orig <- exemplo_mutate %>% 
  group_by(oldbalanceOrg, newbalanceOrig) %>% 
  summarise(count = n())

acumulado <- exemplo_mutate %>% 
  group_by(nameDest, step) %>% 
  mutate(quant_Orig = n(), acumulado = row_number())

#LOGISTIC REGRESSION BINOMIAL

#Construct the model
mod <- glm(isFraud ~ type + amount,
           family = binomial(link = 'logit'), data = Fraud)

#Verify outliers and leverage poitns
plot(mod, which = 5)
summary(stdres(mod))

#Build the model = Logist Regression
library(caTools)

#Atribuir um nome ao conjunto de dados (80% para treino e 20% para teste)
sample <- sample.split(Fraud$isFraud, SplitRatio = 0.8)

#training data 
train <- subset(Fraud, sample == TRUE)
test <- subset(Fraud, sample == FALSE)

#Applying - Model 1
mod1 <- glm(isFraud ~ type + amount + oldbalanceOrg + newbalanceOrig + oldbalanceDest + newbalanceDest, family = binomial(link = 'logit'), data = train)
summary(mod1)

#Desenvolved Prediction for treinament base 
predicttrain = predict(mod1,type= ”response”)
summary(predicttrain)
tapply(predicttrain, train$isFraud, mean)

#Confusion Matrix for threshlod of 0.2 
table(train$isFraud,predicttrain>0.2)

#Confusion Matrix for threshlod of 0.5
table(train$isFraud,predicttrain>0.5)

#The limit the base train is 0.5 

#Results: 
#Precision: 0.999402 
#Recall: 0.999765 
#F1 Score: 0.999583 
#Accuracy: 0.999171

#Now, run the base test in Confunsion Matrix 
mod2 <- glm(isFraud ~ type + amount + oldbalanceOrg + newbalanceOrig + oldbalanceDest + newbalanceDest, family = binomial(link = 'logit'), data = test)
summary(mod2)

#Desenvolved Prediction for test base 
predicttest = predict(mod2,type= ”response”)
summary(predicttest)
tapply(predicttest, test$isFraud, mean)

#Confusion Matrix for threshlod of 0.2 
table(test$isFraud,predicttest>0.2)

#Confusion Matrix for threshlod of 0.5
table(test$isFraud,predicttest>0.5)

#Confusion Matrix for threshlod of 0.7
table(test$isFraud,predicttest>0.7)

#Verify probability 
prop.table(table(train$Survived))

#Applying - Model 2 
mod2 <- glm(isFraud ~ type + amount + oldbalanceOrg + newbalanceOrig + oldbalanceDest + newbalanceDest, family = binomial(link = 'logit'), data = test)

#Results: 
#Precision: 0.999327
#Recall: 0.9997159
#F1 Score: 0.9995218
#Accuracy: 0.9990444

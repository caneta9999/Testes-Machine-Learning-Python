#Importando bibliotecas
#install.packages("mlbench")
library(caret)
library(mlbench)

#Lendo os dados
data(PimaIndiansDiabetes)
data <- PimaIndiansDiabetes
#data[,1:8] = as.matrix(apply(data[,1:8], 2, function(x) (x-min(x))/(max(x) - min(x))))

#Separando os dados em base de treinamento e teste
division <- createDataPartition(data$diabetes,p = .75, list = FALSE)
train <- data[division,]
test <- data[-division,]
print(head(train))
print(head(test))

#Logistic Regression
set.seed(100)
model <- glm( diabetes ~., data = train, family=binomial(link='logit'))
print(summary(model))

#Previsão
prediction <- ifelse(predict(model, test, type="response")>0.5,"pos","neg")
print(paste('Accuracy',1 - mean(prediction != test$diabetes)))
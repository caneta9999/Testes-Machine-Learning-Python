#Importando biliotecas necessárias
#install.packages("caret")
#install.packages("mlbench")
#install.packages("randomForest")
library(caret)
library(mlbench)
library(randomForest)

#Carregando os dados da base de dados Glass, dataset que vem junto com o R
data(Glass)
data <- Glass

#Visualizar os dados
print(data)

#Verificar se há alguma relação perceptível entre a variável Na e Mg
plot(data[,"Na"],data[,"Mg"], xlab="Na", ylab="Mg")

#Receber um sumário estátistico da base de dados
print(summary(data))

#Descobrir o registro que tem a maior quantidade de NA
print(rownames(data)[which.max(data$Type)])

#Normalizar os dados
data[,1:9] = as.matrix(apply(data[,1:9], 2, function(x) (x-min(x))/(max(x) - min(x))))

#Random forest
set.seed(100)
rf <- train(Type~.,data=data,method='rf',metric='Accuracy')
print(rf)
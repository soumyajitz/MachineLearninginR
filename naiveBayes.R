library(lattice)
library(ggplot2)
library(caret)
library(klaR)
library(ElemStatLearn)
dataset <- read.csv(file="/Users/soumyajitsarkar/Desktop/Models/738projectTrain.csv",head=TRUE,sep=",")
trainset <- dataset[1:50000,]
trainset$V2 <- as.character(trainset$V2)
trainset$V2[trainset$V2 == "0"] <- "FALSE"
trainset$V2[trainset$V2 == "1"] <- "TRUE"
testset <- dataset[50001:60000,]
testset$V2[testset$V2 == "0"] <- "FALSE"
testset$V2[testset$V2 == "1"] <- "TRUE"
attach(trainset)
cv_opts = trainControl(method="cv", number=10)
# modelNBayes <- naiveBayes(V2~.,data=trainset)
# pred_NB = predict(modelNBayes,testset[-2])
# resultsNB <- data.frame(actual = testset$V2, prediction = pred_NB)
modelNB = train(V2~.,data=trainset,method='nb',trControl=cv_opts)
pred_NB = predict(modelNB,testset[-2])
confusionMatrix(pred_NB, testset[,2], positive='TRUE')
resultsNB <- data.frame(actual = testset$V2, prediction = pred_NB)


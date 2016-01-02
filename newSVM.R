library(lattice)
library(ggplot2)
library(caret)
set.seed(200)
dataset <- read.csv(file="/Users/soumyajitsarkar/Desktop/Models/738projectTrain.csv",head=TRUE,sep=",")
trainset <- dataset[1:1000,]
trainset$V2 <- as.character(trainset$V2)
trainset$V2[trainset$V2 == "0"] <- "FALSE"
trainset$V2[trainset$V2 == "1"] <- "TRUE"
testset <- dataset[1:nrow(dataset),]
testset$V2[testset$V2 == "0"] <- "FALSE"
testset$V2[testset$V2 == "1"] <- "TRUE"
attach(trainset)
cv_opts = trainControl(method="cv", number=5)
results_svm = train(V2~., data=trainset, method="svmLinear",preProcess="range", trControl=cv_opts, tuneLength=5)
results_svm
preds_svm = predict(results_svm, testset[,-2])
confusionMatrix(preds_svm, testset[,2], positive='1')
confusionMatrix(results_svm)
resultsSVM <- data.frame(actual = testset$V2, prediction = preds_svm)

#save and load model
save(results_svm, file="svmmodel703.Rdata")
load("svmmodel.RData")


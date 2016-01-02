library(lattice)
library(ggplot2)
library(caret)
library(nnet)
dataset <- read.csv(file="/Users/soumyajitsarkar/Desktop/Models/738projectTrain.csv",head=TRUE,sep=",")

trainset <- dataset[1:nrow(dataset),]
trainset$V2 <- as.character(trainset$V2)
trainset$V2[trainset$V2 == "0"] <- "NO"
trainset$V2[trainset$V2 == "1"] <- "YES"
#testset <- read.csv(file="/Users/soumyajitsarkar/Desktop/Models/738projectTest.csv",head=TRUE,sep=",")
testset<- dataset[1:nrow(dataset),]
testset$V2[testset$V2 == "0"] <- "NO"
testset$V2[testset$V2 == "1"] <- "YES"
attach(trainset)
cv_opts = trainControl(method="cv", number=10)
results_nnet = train(V2~., data=trainset, method="avNNet", trControl=cv_opts, preProcess="range",tuneLength=5, trace=F, maxit=1000)
plot(results_nnet, rep = "best")
results_nnet
preds_nnet = predict(results_nnet, testset[,-2]) 
confusionMatrix(preds_nnet, testset[,2],positive = 'TRUE')
#confusionMatrix(results_nnet)
results <- data.frame(actual = testset$V2, prediction = preds_nnet)

res <- results

#save and load model
save(results_nnet, file="~/results_nnetmodel706-5k.Rdata")


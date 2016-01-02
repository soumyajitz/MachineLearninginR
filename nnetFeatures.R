library(nnet)
library(lattice)
library(ggplot2)
library(caret)
dataset <- read.csv(file="/Users/soumyajitsarkar/Desktop/Models/738projectTrain.csv",head=TRUE,sep=",")
trainset <- dataset[1:nrow(dataset),]
trainset$V2 <- as.character(trainset$V2)
trainset$V2[trainset$V2 == "0"] <- "FALSE"
trainset$V2[trainset$V2 == "1"] <- "TRUE"
testset <- dataset[1:nrow(dataset),]
testset$V2[testset$V2 == "0"] <- "FALSE"
testset$V2[testset$V2 == "1"] <- "TRUE"
attach(trainset)
fitControl <- trainControl(method="cv", number = 5, classProbs = TRUE)
fit_nnet <- train(V2~.,
                  data = trainset,
                  method = 'nnet',
                  preProcess = 'range',
                  trControl = fitControl,
                  tuneGrid = expand.grid(.size = c(1, 5, 10),
                                         .decay = c(0, 0.001, 0.1)),
                  trace = FALSE,  # switch for tracing optimization. Default TRUE
                  maxit = 1000)   # maximum number of iterations. Default 100
predict_nnet <- predict(fit_nnet, newdata = testset[,-2], type = 'raw')
results <- data.frame(actual = testset$V2, prediction = predict_nnet)
confusionMatrix(predict_nnet,testset[,2], positive = 'TRUE')
importance_nnet2 <- varImp(fit_nnet, scale = TRUE)



png(paste0('-', 'importance-nnet3.png'))
plot(importance_nnet2, main = 'Feature importance for NNet')
save(fit_nnet, file="nnetFeaturesmodel.Rdata")

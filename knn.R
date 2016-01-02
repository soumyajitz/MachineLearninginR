library(lattice)
library(ggplot2)
library(caret)
dataset <- read.csv(file="/Users/soumyajitsarkar/Desktop/Models/738projectTrain.csv",head=TRUE,sep=",")
# head(dataset)
# ## extract a set to train and test the NN
trainset <- dataset[1:10000,]
trainset$V2 <- as.character(trainset$V2)
trainset$V2[trainset$V2 == "0"] <- "FALSE"
trainset$V2[trainset$V2 == "1"] <- "TRUE"
testset <- dataset[10001:12000,]
testset$V2[testset$V2 == "0"] <- "FALSE"
testset$V2[testset$V2 == "1"] <- "TRUE"
attach(trainset)
# normalize1 <- function(x){
#   return ( (x-min(x))/(max(x)-min(x)) )
# }
# 
# normalizeddata <- as.data.frame(lapply(trainset[1:52],normalize1))
# traindata <- normalizeddata[1:80,]
# testdata <- normalizeddata[81:100,]
# trainLabels <- trainset[1:80,2]
# testLabels <- trainset[81:100,2]
# library(class)
# prediction <- knn(train=traindata,test=testdata,cl=trainLabels,k = 10)


set.seed(1234)
cv_opts = trainControl(method="cv", number=10)
knn_opts = data.frame(.k=c(1,seq(3, 52, 3)))
results_knn = train(V2~., data=trainset, method="knn",preProcess="range", trControl=cv_opts,tuneGrid = knn_opts)
results_knn
preds_knn = predict(results_knn, testset[,-2]) 
confusionMatrix(preds_knn, testset[,2],positive = 'TRUE')
results <- data.frame(actual = testset$V2, prediction = preds_knn)


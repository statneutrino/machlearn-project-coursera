#Download testing and training data
download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="pml-training.csv")
training <- read.csv("pml-training.csv")
download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile="pml-testing.csv")
testing <- read.csv("pml-testing.csv")

library(caret)

testNA <- sapply(testing, function(x) sum(is.na(x)))
test.covariates < names(testNA[testNA==0])

testNA <- sapply(testing, function(x) sum(is.na(x)))
test.covariates < names(testNA[testNA==0])
#numNA <- sapply(training, function(x) sum(is.na(x)))
#covariates <- names(sumNA[sumNA==0])

fit.train <- subset(training, select = c(test.covariates[-c(1:7,-60)], "classe"))

library(caret); set.seed(1111)

inTrain <- createDataPartition(y=fit.train$classe, p=0.7, list=FALSE)
trainFit <- fit.train[inTrain,]
testFit <- fit.train[-inTrain,]


RFvarImp <- createDataPartition(y = fit.train$classe, p = 0.1, list = F)
VarImpModel <- train(classe ~ ., method = "rf", fit.train[RFVarImp,], ntree = 50, importance = TRUE)
vi <- varImp(VarImpModel)
Imp20 <- rownames(vi$importance[1:20,])

rfModel <- train(classe ~ ., method = "rf", trainFit[,c(Imp20, "classe")], ntree = 100, importance = TRUE)
saveRDS(rfModel, "rfModel.rds")

fit.predict <- predict(rfModel,testFit)

confusionMatrix(testFit$classe,fit.predict)

answers = rep("A", 20)
  
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }

pml_write_files(answers)

nswers = predict(rfModel,testing)

vi <- varImp(VarImpModel, scale=FALSE)
vi <- varImp(VarImpModel, scale=FALSE)
plot(vi)
print(vi$importance[1:20])
Imp20 <- rownames(vi$importance[1:20,])
print(vi$importance[1:20])

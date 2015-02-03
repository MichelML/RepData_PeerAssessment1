#packages:caret,randomForest,rpart,rpart.plot,e1071
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
set.seed(1210)
train_set <-read.csv("malearntrain.csv", na.strings=c("NA","#DIV/0!",
                                                                                      ""))
test_set <- read.csv("machlearntest.csv", na.strings=c("NA","#DIV/0!", "
"))
dim(train_set)
dim(test_set)
train_set<-train_set[,colSums(is.na(train_set)) == 0]
test_set <-test_set[,colSums(is.na(test_set)) == 0]
train_set <-train_set[,-c(1:7)]
test_set <-test_set[,-c(1:7)]
dim(train_set)
dim(test_set)
head(train_set)
head(test_set)
subsamples <- createDataPartition(y=train_set$classe, p=0.75, list=FALSE)
subTraining <- train_set[subsamples, ]
subTesting <- train_set[-subsamples, ]
dim(subTraining)
dim(subTesting)
head(subTraining)
head(subTesting)
plot(subTraining$classe, col="red", main="Bar Plot of classe levels", xlab="classe levels", ylab="Frequency")

mod1 <- rpart(classe ~ ., data=subTraining, method="class")
# Predicting:
pred1 <- predict(mod1, subTesting, type = "class")
# Plot of the Decision Tree
rpart.plot(mod1, main="classe Tree", extra=102, under=TRUE, faclen=0)



mod2 <- randomForest(classe ~. , data=subTraining, method="class")
# Predicting:
pred2 <- predict(mod2, subTesting, type = "class")
# Test results on subTesting data set:
confusionMatrix(pred2, subTesting$classe)

pred_subm <- predict(mod2, test_set, type="class")
pred_subm

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred_subm)


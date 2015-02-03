library(caret)
library(kernlab)
data(spam)
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)
dim(testing)
set.seed(333)
modelFit<-train(type~.,data=training,method="glm")
modelFit$finalModel
predictions<-predict(modelFit,newdata=testing)
confusionMatrix(predictions,testing$type)
#k-fold
folds<-createFolds(y=spam$type,k=10,list=TRUE,returnTrain=TRUE)
sapply(folds,length)
#Return test
folds<-createFolds(y=spam$type,k=10,list=TRUE,returnTrain=FALSE)
sapply(folds,length)
#createResample
folds<-createResample(y=spam$type,times=10,list=TRUE)
#create time slices
tme<-1:1000
folds<-createTimeSlices(y=tme,initialWindow=20,horizon=20)
folds$train[[1]]
folds$test[[1]]

#there is more train options -- see args(train.default)

#plotting predictors
library(ISLR);library(ggplot2);library(caret)
data(Wage)
summary(Wage)
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
dim(training)
dim(testing)
featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot="pairs")
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
qq<-qplot(age,wage,colour=education,data=training)
qq+geom_smooth(method="lm",formula=y~x)
library(Hmisc)
cutWage<-cut2(training$wage,g=3)
table(cutWage)
library(grid)
p1<-qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot"))
p2<-qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)
t1<-table(cutWage,training$jobclass)
prop.table(t1,1)
qplot(wage,colour=education,data=training,geom="density")


#basic preprocessing
data(spam)
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)
dim(testing)
hist(training$capitalAve,main="",xlab="ave capital run length")
mean(training$capitalAve)
sd(training$capitalAve)
###standardizing
library(RANN)
trainCap<-training$capitalAve
trainCap_standard<-(trainCap- mean(trainCap))/sd(trainCap)
testCap<-testing$capitalAve
testCap_standard<-(testCap- mean(testCap))/sd(testCap)
#tou can use preProcess also
preObj<-preProcess(training[,-58],method=c("center","scale"))
trainCap<-predict(preObj,training[,-58])$capitalAve
testCap<-predict(preObj,testing[,-58])$capitalAve
##Box-Cox transforms can also be used to standardize
##imputing data (removing missing values)
training$capAve<-training$capitalAve
selectNA<-rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA]<-NA
#impute and standardize
preObj<-preProcess(training[,-58],method="knnImpute")
capAve<-predict(preObj,training[,-58])$capAve
capAvetruth<-training$capitalAve
capAvetruth<-(capAvetruth-mean(capAvetruth))/sd(capAvetruth)


###
modFit<-train(Species~.,method="rpart",data=iris)
print(modFit$finalModel)
plot(modFit$finalModel,uniform=TRUE)
text(modFit$finalModel,use.n=TRUE,all=TRUE,cex=.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)
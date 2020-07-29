install.packages("rpart")
install.packages("rpart.plot")
install.packages("readxl")

library(openxlsx)
library(readxl)
url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/00428/Immunotherapy.xlsx"

Immunotherapy<-as.data.frame(read.xlsx(url))
table(Immunotherapy$Result_of_Treatment)
Immunotherapy$Result_of_Treatment<-as.factor(Immunotherapy$Result_of_Treatment)
Immunotherapy$Result_of_Treatment <- relevel(Immunotherapy$Result_of_Treatment, ref = "1")

library(clusterSim)
Immunotherapy[,-8]<-data.Normalization(Immunotherapy[,-8],type="n4",normalization="column")


library(caret)
set.seed(90)
trainIndexes <- createDataPartition(y = Immunotherapy$Result_of_Treatment, p = .70, list = FALSE) 
trainIndexes[1:20]
trainSet<-Immunotherapy[trainIndexes,]
testSet<-Immunotherapy[-trainIndexes,]
table(Immunotherapy$Result_of_Treatment)
table(trainSet$Result_of_Treatment)
table(testSet$Result_of_Treatment)

testAttributes<-testSet[,-8]
testClass<-testSet[[8]]
trainAttributes<-trainSet[,-8]
trainClass<-trainSet[[8]]






library(rpart)
library(rpart.plot)
ct<- rpart(Result_of_Treatment~., trainSet, method="class")
ct
rpart.plot(ct,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
pre <- predict(ct,testAttributes,type="class")
pretable <- table(pre,testSet$Result_of_Treatment)
pretable
accuracy<-sum(diag(pretable)/sum(pretable))
accuracy



library(C50)
ct2<-C5.0(trainAttributes,trainClass)
ct2
summary(ct2)
pre <- predict(ct2,testAttributes,type='class')
pretable <- table(pre,testClass)
pretable
accuracy<-sum(diag(pretable)/sum(pretable))
accuracy

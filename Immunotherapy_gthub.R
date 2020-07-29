install.packages("readxl")
install.packages("corrplot")
library(readxl)
library(corrplot)
library(openxlsx)

url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/00428/Immunotherapy.xlsx"

Immunotherapy<-as.data.frame(read.xlsx(url))
help(read_excel)
table(Immunotherapy$Result_of_Treatment)
b<-cor(Immunotherapy,use = "all.obs",method="pearson")

corrplot(b,method ="number",is.corr = FALSE,diag = FALSE,col = "black",tl.col = "black")
Immunotherapy$Result_of_Treatment<-as.factor(Immunotherapy$Result_of_Treatment)

levels(Immunotherapy$Result_of_Treatment)
table(Immunotherapy$Result_of_Treatment)
Immunotherapy$Result_of_Treatment <- relevel(Immunotherapy$Result_of_Treatment, ref = "1")


library(clusterSim)
Immunotherapy[,-8] <-data.Normalization(Immunotherapy[,-8],type="n4",normalization="column")


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


k_value<-3
help(knn)
library(class)
set.seed(1)
model <- knn(trainAttributes, testAttributes, trainClass,k = k_value)

library(caret)
karMatris <- confusionMatrix(data = model, reference = testClass, mode = "everything")
karMatris


k_value<- c(1:20)
k_value
max<-0
max_K<-0
for(i in k_value){
  set.seed(1)
  model <- knn(trainAttributes, testAttributes, trainClass, k = i)
  table2 <- confusionMatrix(data = model, reference = testClass, mode = "everything")
  Acc<-unname(table2$overall["Accuracy"])
  print(paste0("While k value is : ",i," the accuracy is: ", Acc))
  if (Acc>max)
  {
    max<-Acc
    max_K<-i
  }
}





##################################################
########### Mithat Yavuzarslan - 2018 ############
########### mail: mithatyavuzarslan@gmail.com#####
##################################################

##### Dataset retrieved from UCI Machine Learning Repository: ########
#Source:

#Name: Fahime Khozeimeh, MD
#email: fahime.khozeime '@' yahoo.com
#institution: Faculty of Medicine, Mashhad University of Medical Sciences, Mashhad, Iran.

#Name: Pouran Layegh, Professor of Dermatology
#email: layeghpo '@' mums.ac.ir
#institution: Mashhad University of Medical Sciences, Mashhad, Iran
#website: http://research.mums.ac.ir/webdocument/load.action?webdocument_code=8001&masterCode=8000703

#Name:Roohallah Alizadehsani, PhD student
#email: alizadeh_roohallah '@' yahoo.com
#institution: Institute for Intelligent Systems Research and Innovation (IISRI), Deakin University, Victoria 3217, Australia.
#website: http://ce.sharif.ir/~ralizadeh/

#Name: Mohamad Roshanzamir, PhD candidate
#email: mohamad.roshanzamir '@' ec.iut.ac.ir
#institution: Electrical and Computer Engineering, Isfahan University of Technology, Isfahan, Iran.
###################################################



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

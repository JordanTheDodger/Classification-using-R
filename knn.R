library(ggplot2)
library(readr)
library(class)
library(gmodels)

library(e1071)
library(ROCR)
library(caret)
library(caTools)

brcan<-read.csv(file.choose())
str(brcan)
head(brcan)
colnames(brcan)

#EDA
brcan[rowSums(is.na(brcan))>0,] #4 NA values

#replace NA with mean values
brcan$alkphos=ifelse(is.na(brcan$alkphos),
                     ave(brcan$alkphos,FUN = function(x)mean(x,na.rm = TRUE)),brcan$alkphos)
#check count for missing values
sum(is.na(brcan))
table(is.na(brcan))

#factoring catego > numerical
brcan$is_patient <- factor(brcan$is_patient)
brcan$gender=factor(x=brcan$gender,levels = c('Female','Male'),labels=c(0,1))

brcan_gender <- brcan$gender
brcan <- brcan[-2]
brcan$gender = brcan_gender

#split
set.seed(11)
brcan_sample = sample.split(brcan$is_patient, SplitRatio = 0.8)
training_data = subset(brcan, brcan_sample == TRUE)
test_data = subset(brcan, brcan_sample == FALSE)

#scaling the data
training_data[,1:9] = scale(training_data[,1:9])
test_data[,1:9] = scale(test_data[,1:9])

#Knn model
#k=3
y_pred_knn_3 = knn(train = training_data[,-10], test = test_data[,-10], cl = training_data[,10], k=3)
cfn_matrix3 = confusionMatrix(as.factor(test_data[,10]),
                             y_pred_knn_3)
cfn_matrix3

#k=5
y_pred_knn_5 = knn(train = training_data[,-10], test = test_data[,-10], cl = training_data[,10], k=5)
cfn_matrix5 = confusionMatrix(as.factor(test_data[,10]),
                             y_pred_knn_3)
cfn_matrix5

#k=7
y_pred_knn_7 = knn(train = training_data[,-10], test = test_data[,-10], cl = training_data[,10], k=7)
cfn_matrix7 = confusionMatrix(as.factor(test_data[,10]),
                             y_pred_knn_3)
cfn_matrix7



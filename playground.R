library(ggplot2)
library(readr)
library(class)
library(gmodels)

library(e1071)
library(ROCR)
library(caret)
library(caTools)

brcan<-read.csv('E:/COSC 757 Data Mining/Assignments/4/Liver Patient Dataset.csv')
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
brcan_sample = sample.split(brcan$is_patient, SplitRatio = 0.7)
training_data = subset(brcan, brcan_sample == TRUE)
test_data = subset(brcan, brcan_sample == FALSE)

#scaling the data
training_data[,1:9] = scale(training_data[,1:9])
test_data[,1:9] = scale(test_data[,1:9])

#making training set
predictor = glm(is_patient~., family = binomial, data = training_data)

#predict test results
liver_pat_pred = predict(predictor, ttype = 'response', newdata = test_data[-10])
str(liver_pat_pred)
summary(liver_pat_pred)
y_predictor = ifelse(liver_pat_pred > 0.5,2,1)

#Confusion Matrix
table(test_data[,10], y_predictor)
str(test_data[,10])
str(y_predictor)
levels(test_data[,10])
#factoring
#table(factor(y_predictor, levels= 1:175), factor(test_data[,10], levels= 1:175))

cfn_matrix = confusionMatrix(factor(y_predictor, levels = 1:175),
                factor(test_data[,10], levels = 1:175))
cfn_matrix

#Knn model
#k=3
#test_data_cpy <- as.factor(test_data[,10])
#train_data_cpy <- as.factor(training_data[,10])
#y_pred_knn_3 = knn(train = train_data_cpy[,10], test = test_data_cpy[,10], cl = training_data[,10], k=3)


y_pred_knn_3 = knn(train = training_data[,-10], test = test_data[,10], cl = training_data[,10], k=3)

str(test_data)
head(test_data)
summary(test_data$gender)
summary(y_predictor)
str(y_predictor)
str(test_data[,10])

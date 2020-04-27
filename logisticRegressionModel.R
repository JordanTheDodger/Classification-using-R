library(ROCR)
library(caret)
library(caTools)
library(e1071)
library(corrplot)
library(readr)
library(pROC)

livpd <- read.csv(file.choose())
head(livpd)

livpd_cpy <- livpd
livpd_cpy$age <- as.factor(livpd_cpy$age)
livpd_cpy$tot_proteins <- as.factor(livpd_cpy$tot_proteins)
livpd_cpy$albumin <- as.factor(livpd_cpy$albumin)
livpd_cpy$ag_ratio <- as.factor(livpd_cpy$ag_ratio)


livpd_cpy$is_patient <- as.factor(livpd_cpy$is_patient)
livpd$is_patient <- as.factor(livpd$is_patient)
str(livpd_cpy)
str(livpd)
#EDA
livpd_cpy[rowSums(is.na(livpd_cpy))>0,] #4 NA values
livpd[rowSums(is.na(livpd))>0,] #4 NA values
#replace with mean
livpd$alkphos[is.na(livpd$alkphos)] <- median(livpd$alkphos, na.rm = T)
sum(is.na(livpd))

#label
xtabs(~is_patient + gender, data = livpd)

#split the data
set.seed(2)
train_1.df <- livpd[1:as.integer(0.70*nrow(livpd)),]
names(train_1.df)
test_1.df <- livpd[-c(1:as.integer(0.70*nrow(livpd))),]
nrow(train_1.df)
nrow(test_1.df)

#logistic regression
#model1
mod_f1 <- is_patient ~ age + tot_bilirubin + direct_bilirubin + tot_proteins + albumin + ag_ratio+ sgpt + sgot + alkphos +  is_patient + gender
model1 <- glm(mod_f1 , data = train_1.df , family = binomial(link = "logit"))
summary(model1)
#pred_1 <- predict(model1, test_1.df, type = "response")
#pred_logreg_1 <- ifelse(pred_1 >= 0.5,1,0)


#Try2
#split the data
split_livpd <- sample.split(livpd_cpy, SplitRatio = 0.7)
train_split <- subset(livpd_cpy, split_livpd == "TRUE")
test_split <- subset(livpd_cpy, split_livpd == "FALSE")
nrow(train_split)
nrow(test_split)
model2 <- glm(is_patient ~ .-gender, train_split, family = "binomial")
summary(model2)

pred_glm_mdl2 <- predict(model2,test_split, type = "response")
pred_glm_mdl2
pred_logreg_2 <- ifelse(pred_glm_mdl2 >= 0.5,1,0)
pred_logreg_2
(table(ActualValue=test_split$is_patient, PredictedValue=pred_glm_mdl2>= 0.5))

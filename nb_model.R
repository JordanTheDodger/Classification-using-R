df<-read.csv('E:/COSC 757 Data Mining/Assignments/4/Liver Patient Dataset.csv')
colnames(df)

#EDA
df[rowSums(is.na(df))>0,] #4 NA values
#replace with mean
df$alkphos[is.na(df$alkphos)] <- median(df$alkphos, na.rm = T)
sum(is.na(df))

#label
str(df)
summary(df$gender)
head(df$gender)
df_cpy <- df
names(df_cpy)
df_cpy <- df_cpy[,c(1,3,4,5,6,7,8,9,10,2,11)]
df_cpy$gender <- NULL
names(df_cpy)

#split the dataset
set.seed(2)
id <- sample(2, nrow(df_cpy), prob = c(0.7,0.3), replace = T)
train_df <- df_cpy[id==1,]
test_df <- df_cpy[id==2,]
nrow(train_df)
nrow(test_df)

#naive bayes
library(e1071)
library(caret)

colnames(train_df)
nb_df <- naiveBayes(as.factor(is_patient) ~ ., data = train_df)
nb_df

predictor_df <- predict(nb_df, test_df)
confusionMatrix(table(predictor_df, test_df$is_patient))

nrow(nb_df)
str(test_df)

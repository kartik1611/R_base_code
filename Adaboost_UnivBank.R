rm(list=ls(all.names=TRUE))
setwd("C:/INSOFE (COACHING)/MACHINE LEARNING/R/ENSEMBLE (Random Forest)")

# the Univ_bank dataset has 14 variables and 5000 records. Use loan as target variable. 

# read the dataset into R
U_Bank <-read.csv(file="UniversalBank.csv",header=T,sep=",",fill=T)

# drop unnecessary the attributes (ID & ZIP Code)
str(U_Bank)
U_Bank <-U_Bank[,-c(1,5)]
str(U_Bank)

# subset data into numeric and categorical
U_Bank_Num <- subset(x = U_Bank, select = c(Age, Experience, Income, Family, CCAvg, Mortgage))
U_Bank_Cat <- subset(x = U_Bank, select = -c(Age, Experience, Income, Family, CCAvg, Mortgage))

# convert categorical attributes into factors
U_Bank_Cat <- data.frame(apply(X = U_Bank_Cat, MARGIN = 2, FUN = as.factor))

# convert numerical attributes into numerics
U_Bank_Num <- data.frame(apply(X = U_Bank_Num, MARGIN = 2, FUN = as.character))
U_Bank_Num <- data.frame(apply(X = U_Bank_Num, MARGIN = 2, FUN = as.numeric))

# standardize the numeric data using 'Range' method. 
library(vegan)
U_Bank_num_std <- decostand(x = U_Bank_Num,method = "range") 
summary(U_Bank_num_std)

# combine the converted categorical and numerical dataframes
U_Bank_merged <- cbind(U_Bank_num_std,U_Bank_Cat)

# dummify "Education" attribute 
library(dummies)
str(U_Bank_merged)
Edu_Dummy <-dummy(U_Bank_merged$Education)
head(U_Bank_merged)
U_Bank_merged <-data.frame(U_Bank_merged,Edu_Dummy) # or also cbind
head(U_Bank_merged)

# remove the original "Education" attribute
str(U_Bank_merged)
U_Bank_merged<- U_Bank_merged[,-c(6)]
head(U_Bank_merged)

# split data into train and test
library(caret)
Train <-createDataPartition(U_Bank_merged$Personal.Loan, p=0.7,list=FALSE)
Training <- U_Bank_merged[Train,]
Testing <- U_Bank_merged[-Train,]
nrow(Training)
nrow(Testing)

# rows <- seq(from = 1, to = nrow(U_Bank_merged), by = 1)
# set.seed(2020)
# trainrows <- sample(x = rows, size = nrow(U_Bank_merged) * 0.7)
# trainR <- U_Bank1_std[trainrows,] #all rows in trainrows & all columns of parent dataset
# testR <- U_Bank1_std[-trainrows,]

# build the classification model using Adaboost
library(ada) 
x = subset(Training, select = -Personal.Loan) 
y = as.factor(Training$Personal.Loan) 
a = subset(Testing, select = -Personal.Loan) 
b = as.factor(Testing$Personal.Loan) 

# 20 Iterations 
model = ada(x, y, iter=20, loss="logistic") 
model

# predict the values using model on test data sets. 
pred = predict(model, a);pred 

# calculate precision, recall and accuracy 
result <- table(pred, b);result # 0(-ve) and 1(+ve)
accuracy <- sum(diag(result))/sum(result)*100;accuracy
recall <- ((result[2,2])/(result[2,2]+result[1,2])*100);recall
precision <-((result[2,2])/(result[2,2]+result[2,1])*100);precision

# experiment with different number of iterations and find the best. 


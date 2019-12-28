rm(list=ls(all=T))
setwd("D:\\Gautham PC\\D\\Insofe\\Batch 18\\20160717_Batch18_Clustering and PCA_LabActivity\\")

#R code to demonstrate Principal component analysis
# Attaching a specific library and understanding the data

# "attitude" Data Description
# From a survey of the clerical employees of a large financial organization, 
# the data are aggregated from the questionnaires of the approximately 35
# employees for each of 30 (randomly selected) departments. 
# The numbers give the percent proportion of favourable responses to seven
# questions in each department.

data(attitude)
names(attitude)
summary(attitude)
str(attitude)
attach(attitude)

#constructing a linear regression model
model1= lm(rating ~ complaints+
             privileges+
             learning+raises+
             critical+
             advance)

# Check the summary of the model
summary(model1)
# Correlation of the data
cor(attitude[,-1])
# Removing the target attribute
data <- attitude[,-c(1)]

library(vegan)
# Normalizing the data
data = decostand(data,method = "range")
# Running a PCA and analyzing the values
pca_data <- princomp(data)
# Summary of the PCA model
summary(pca_data)
plot(pca_data)
print(pca_data)
# 
pca_data$loadings[,]

#creating the data set with four components
data2<-data.frame(rating,pca_data$scores[,1:4])
# Building the linear regression model using the PCA components
model2 = lm(rating ~ .,data=data2)
summary(model2)

# Excercise
rm(list=ls(all=T))
library(vegan)
# Loading the Cereals data
data = read.csv("Cereals.csv",header=T)
data = as.matrix(data)
# Normalizing the data
data <- as.matrix(decostand(data,method = "range")) 
# Ignoring the NA values
data<-na.omit(data)
# Deleting the dependent variable
data <- data[,-c(1)]
# Analysing the correlation analysis
cor(data)
names(data)
# Running the PCA on the data
pca_data <- princomp(data)
summary(pca_data)
plot(pca_data)
print(pca_data)

# Clustering based on the PCA components
fit = kmeans(pca_data$scores,centers=3)
fit
rm(list = ls())
setwd("F:\\TA\\Labs\\AML\\Batch18\\Full Lab Day")

mydata = read.table("machine_data.txt", sep = ",", header = F)
write.csv(mydata,file = "machine_data.csv", row.names = F)

my_data = read.csv("machine_data.csv", header = T)
names(my_data) = c("vendor_name", "model_name", 
                   "myct", "mmin", "mmax", 
                   "cach", "chmin", "chmax", 
                   "prp", "erp")
str(my_data)

for (i in 3:10){
  my_data[,i] = as.numeric(my_data[,i])
}

my_data_num = my_data[,-c(1,2)]
str(my_data_num)

library(caret)
intrain = createDataPartition(y=my_data_num$erp, p=0.7, list = F)
train = my_data_num[intrain, ]
test = my_data_num[-intrain, ]

### Linear regression
lm = lm(erp~., data = train)
summary(lm)

train_pred = predict(lm, train)
library(DMwR)
regr.eval(train[,8], train_pred)

test_pred = predict(lm, test)
regr.eval(test[,8], test_pred)

### Ridge Regression
library(glmnet)
lm_ridge = glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 0)
summary(lm_ridge)
lm_ridge$lambda

train_pred = predict(lm_ridge, as.matrix(train[,-8]))
regr.eval(train[,8], train_pred)
test_pred = predict(lm_ridge, as.matrix(test[,-8]))
regr.eval(test[,8], test_pred)

### Lasso Regression
lm_lasso = glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 1)
summary(lm_lasso)

train_pred = predict(lm_lasso, as.matrix(train[,-8]))
regr.eval(train[,8], train_pred)
test_pred = predict(lm_lasso, as.matrix(test[,-8]))
regr.eval(test[,8], test_pred)

# Lasso Regression  using glmnet - L1 norm
library(glmnet)
# fit model
fit1 <- glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 1)

plot(fit1,xvar="lambda",label=TRUE)

#Model Selection
coef(fit1)
cv.lasso=cv.glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]))
plot(cv.lasso)
coef(cv.lasso)


# Ridge Regression  using glmnet  - L2 norm
library(glmnet)
# fit model
fit2 <- glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 0)
plot(fit2,xvar="lambda",label=TRUE)


#Model Selection
coef(fit2) 
cv.ridge=cv.glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 0)
plot(cv.ridge)
coef(cv.ridge)

######
library(h2o)
h2o.init()
train.hex = as.h2o(train)
test.hex = as.h2o(test)

names(train.hex)
y = "erp"
x = setdiff(names(train.hex), y)
nn_reg = h2o.deeplearning(x, y, 
                          training_frame = train.hex, 
                          autoencoder = F,
                          regression_stop = 0.1
                          )
train_pred = predict(nn_reg, train.hex)
train_pred_df = as.data.frame(train_pred)
regr.eval(train[,8], train_pred_df[,1])

test_pred = predict(nn_reg, test.hex)
test_pred_df = as.data.frame(test_pred)
regr.eval(test[,8], test_pred_df[,1])


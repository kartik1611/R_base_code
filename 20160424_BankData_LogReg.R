rm(list = ls(all.names = TRUE))

##Setting the working directory
setwd("E:\\20160424_Batch16_CSE7202c_Lab_Day_Activity")

##Reading the data file into R
Bank<-read.table("bank.txt",header=T,sep=";")

############Data Pre-Processing#############
## To look at the summary and structure of data
str(Bank)  ##Since all the data types of the variables were 
           ##found appropriate, there is no need for data type conversion

##Recode the levels for marital,month and poutcome
Bank$outcome<-ifelse(Bank$y=="yes",1,0)
Bank$outcome<-as.factor(Bank$outcome)
Bank<-Bank[,-17]
##To check the number of missing values
sum(is.na(Bank))

##############Data for model building################
#Split the data into train and test data sets
rows=seq(1,nrow(Bank),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(Bank))/100)
train = Bank[trainRows,] 
test = Bank[-trainRows,]

################Logistic regression model###############
LogReg <- glm(outcome ~ ., data=train, family=binomial)
summary(LogReg)
###Which error metric is appropriate in this case
# train results
prob<-predict(LogReg, type="response")
pred_class <- ifelse(prob> 0.5, 1, 0)
table(train$outcome,pred_class)

conf.mat1 = table(train$outcome,pred_class)
accuracy1 = sum(diag(conf.mat1))/sum(conf.mat1)
precision1 = conf.mat1[2,2]/sum(conf.mat1[,2])
recall1 = conf.mat1[2,2]/sum(conf.mat1[2,])

# Test results 
fitted.results <- predict(LogReg,test,type='response')
fitted.class <- ifelse(fitted.results > 0.5,1,0)
table(test$outcome,fitted.class)

conf.mat2 = table(test$outcome,fitted.class)
accuracy2 = sum(diag(conf.mat2))/sum(conf.mat2)
precision2 = conf.mat2[2,2]/sum(conf.mat2[,2])
recall2 = conf.mat2[2,2]/sum(conf.mat2[2,])


##Variable selection
library(car)
vif(LogReg)
library(MASS)
stepAIC(LogReg)

LogReg_updated<-glm(formula = outcome ~ marital + housing + loan + contact + 
                      day + month + duration + campaign + poutcome, family = binomial, 
                    data = train)

# train results
prob1<-predict(LogReg_updated, type="response")
pred_class1 <- ifelse(prob1> 0.5, 1, 0)
table(train$outcome,pred_class1)

# Test results 
fitted.results1 <- predict(LogReg_updated,test,type='response')
fitted.class1 <- ifelse(fitted.results1 > 0.5,1,0)
table(test$outcome,fitted.class1)



##ROCR curves..
library(ROCR)
library(ggplot2)
predicted <- predict(LogReg_updated,type="response")
prob <- prediction(predicted, train$outcome)
tprfpr <- performance(prob, "tpr", "fpr")
plot(tprfpr)
str(tprfpr)

cutoffs <- data.frame(cut=tprfpr@alpha.values[[1]], fpr=tprfpr@x.values[[1]], 
                      tpr=tprfpr@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.2))

plot(tprfpr, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

tpr <- unlist(slot(tprfpr, "y.values"))
fpr <- unlist(slot(tprfpr, "x.values"))
roc <- data.frame(tpr, fpr)
ggplot(roc) + geom_line(aes(x = fpr, y = tpr)) + 
  geom_abline(intercept=0,slope=1,colour="gray") + 
  ylab("Sensitivity") +    xlab("1 - Specificity")


##Using the ROC curve, obtain the appropriate threshold of probalility for 
## for calculating the error metric
#Ans. From the cutoffs, observe the point where we have good tpr and
#reasonable fpr.
# train results

pred_class1 <- ifelse(prob1> 0.11, 1, 0)
table(train$outcome,pred_class1)

conf.mat1 = table(train$outcome,pred_class1)
accuracy1 = sum(diag(conf.mat1))/sum(conf.mat1)
precision1 = conf.mat1[2,2]/sum(conf.mat1[,2])
recall1 = conf.mat1[2,2]/sum(conf.mat1[2,])

# Test results 
fitted.results1 <- predict(LogReg_updated,test,type='response')
fitted.class1 <- ifelse(fitted.results1 > 0.11,1,0)

conf.mat2 = table(test$outcome,fitted.class1)
accuracy2 = sum(diag(conf.mat1))/sum(conf.mat1)
precision2 = conf.mat1[2,2]/sum(conf.mat1[,2])
recall2 = conf.mat1[2,2]/sum(conf.mat1[2,])






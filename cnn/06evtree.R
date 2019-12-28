rm(list=ls(all=TRUE))
par(mfrow=c(1,1))
setwd("C:/Users/ISE/Desktop/Sims")

source("05largeData.R")

library("rpart")
library(partykit)

dtCart <- as.party(rpart(loan ~ ., data = train))
plot(dtCart)
a <- table(train$loan, predict(dtCart))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100

library(evtree)
set.seed(1090)
evDt <- evtree(loan ~ ., data = train, 
               minbucket = 10, 
               maxdepth = 3)
plot(evDt)

a=table(train$loan, predict(evDt, newdata=train))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100

rm(a,rcEval,rcTest,rcTrain)
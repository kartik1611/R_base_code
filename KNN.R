rm(list=ls(all=TRUE))

library(class)
library(dummies)
library(vegan)

# Bank problem
data = iris

# Standardizing the data
stand_data = decostand(data[,setdiff(names(data), "Species")], "normalize") 
stand_data = data.frame(Species = data$Species, stand_data)

# Splitting into training and testing
rowID =  sample(1:nrow(data), (nrow(data)*0.6))
train = data[rowID, ]
test = data[-rowID, ]
stand_train = stand_data[rowID, ]
stand_test = stand_data[-rowID, ]

# Check how records are split with respect to target attribute.
table(data$Species)
table(train$Species)
table(test$Species)


#N=7
pred = knn(train[,setdiff(names(train), "Species")], 
           test[,setdiff(names(test), "Species")],
           train$Species, k = 7)

cm = table(pred, test$Species)
cm
accu= sum(diag(cm))/sum(cm)
accu
rm(cm, accu, pred)

# With Standardize data 
pred = knn(stand_train[,setdiff(names(stand_train),"Species")],
           stand_test[,setdiff(names(stand_test),"Species")], 
           stand_train$Species, k = 7)

cm = table(pred, stand_test$Species)
stand_accuracy = sum(diag(cm))/sum(cm)
stand_accuracy
rm(pred, cm, stand_accuracy)

#########K-FOLD########
nfolds <- 5
folds <- cut(seq(1,nrow(stand_data)),breaks=nfolds,labels=FALSE)
stand <- sample(c(1:nrow(stand_data)), nrow(stand_data))
stand_data <- stand_data[stand,]
#Perform 10 fold cross validation
accu = c()
for(i in 1:nfolds){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- stand_data[testIndexes, ]
  trainData <- stand_data[-testIndexes, ]
  pred <- knn(trainData[,setdiff(names(trainData), "Species")],
      testData[,setdiff(names(trainData), "Species")],
      trainData$Species, k = 7)
  cm = table(pred, testData$Species)
  cm
  accu = c(accu, sum(diag(cm))/sum(cm))
}
mean(accu)

# Condensing
keep = condense(train[,c(1:4)], train$Species)
keep
nrow(train)
length(keep)

pred=knn(train[keep,c(1:4)], test[,c(1:4)], train$Species[keep], k=1)
cm <- table(pred, test$Species)
cm
accu=sum(diag(cm))/sum(cm)
accu


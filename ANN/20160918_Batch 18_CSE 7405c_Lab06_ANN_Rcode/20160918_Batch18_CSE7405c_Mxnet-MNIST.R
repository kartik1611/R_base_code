rm(list = ls(all=T))

#installation of maxnet library
#http://mxnet.readthedocs.io/en/latest/how_to/build.html?&toperStarEhJUS=1
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")


setwd("C:\\Users\\ISE\\Desktop\\20160918_Batch18_CSE7405c_Lab06_ANN")
library(mxnet)
# require(mxnet)

# Data preparation
train<-read.csv('train_sample.csv')
test<-read.csv('test_sample.csv')
train<-data.matrix(train)
test<-data.matrix(test)
train.x<-train[,-1]
train.y<-train[,1]
train.x<-t(train.x/255)
test_org<-test
test<-test[,-1]
test<-t(test/255)
table(train.y)

# Model Architechture
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=10)
softmax <- mx.symbol.SoftmaxOutput(fc2, name="sm")
devices <- mx.cpu()
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y, 
                                     ctx=devices, 
                                     num.round=25, 
                                     array.batch.size=100,
                                     learning.rate=0.07)
preds <- predict(model, test)
dim(preds)

pred.label <- max.col(t(preds)) - 1
table(pred.label)

table(test_org[,1],pred.label)
sum(diag(table(test_org[,1],pred.label)))/1000



results_test <- cbind(test_org[,1],pred.label)
results_test <- as.data.frame(results_test)

results_test$acc <- rep(0, nrow(results_test))
for (i in 1:nrow(results_test)){
  if (results_test[i,1] == results_test[i,2]){
    results_test[i,3] = 1
  }
}

###**** Visualizing Images ****###
test = read.csv("test_sample.csv")
test_df <- as.data.frame(test)
test_mat <- as.matrix(test_df)
inc <- which(results_test[,3] == 0)

## Color ramp def.
colors <- c('white','black')
cus_col <- colorRampPalette(colors=colors)

## Plot the first 12 images
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
iter = sample(c(1:1000), 12)
for(di in iter)
{
  print(di)
  #   all_img[di+1,] <- apply(train[train[,785]==di,-785],2,sum)
  #   all_img[di+1,] <- all_img[di+1,]/max(all_img[di+1,])*255
  
  z <- array(test_mat[di,-1],dim=c(28,28))
  z <- z[,28:1] ##right side up
  z <- matrix(as.numeric(z), 28, 28)
  image(1:28,1:28,z,main=test_mat[di,1],col=cus_col(256))
}

## Plot the first 12 incorrectly classified images
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
for(di in inc[1:12])
{
  print(di)
  #   all_img[di+1,] <- apply(train[train[,785]==di,-785],2,sum)
  #   all_img[di+1,] <- all_img[di+1,]/max(all_img[di+1,])*255
  
  z <- array(test_mat[di,-1],dim=c(28,28))
  z <- z[,28:1] ##right side up
  z <- matrix(as.numeric(z), 28, 28)
  image(1:28,1:28,z,main=results_test[di,2],col=cus_col(256))
}

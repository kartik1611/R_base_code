# Importing text data into R package:
# To remove previous objects stored in R environment
rm(list=ls(all=TRUE)) 
#load text mining library
library(tm) 
library(e1071)
#sets R's working directory to near where my files are
setwd("D:\\Gautham PC\\D\\Insofe\\Batch 18\\20160813_Batch18_SVM_HMM\\Docs")
#specifies the exact folder where my text file(s) is for analysis with tm.
docs<-Corpus(DirSource("G:\\INSOFE Academics\\11-June\\Docs"), readerControl = list(reader=readPlain)) 
#check what went in
summary(docs)  

#Performing text processing steps:
# Strip any numbers from a text document
docs <- tm_map(docs, removeNumbers)
# Remove punctuation marks from a text document
docs <- tm_map(docs, removePunctuation)
#Strip extra whitespace from a text document
docs <- tm_map(docs , stripWhitespace)
# Remove a set of words from a text document
docs <- tm_map(docs, removeWords, stopwords("english")) 
# this stopword file is at C:\Users\[username]\Documents\R\win-library\2.15.1\library\tm\stopwords 
#Stem words in a text document using Porter's stemming algorithm
docs <- tm_map(docs,stemDocument, language ="english")
#Constructs or coerces to a document-term matrix
dt_matrix <-DocumentTermMatrix(docs) 
# Remove sparse terms from a document-term matrix
dt_matrix <- removeSparseTerms(dt_matrix, 0.75)
#Display detailed information on a document-term matrix
inspect(dt_matrix)


############Data Preparation for Classification Model:
#Store  document-term matrix in a separate data object
matrix=data.frame(inspect(dt_matrix))
data=data.frame(matrix)
#Construct a class variable with levels 1, and 2 for movies, and politics  respectively
data_train=data.frame(rbind(data[1:3,],data[7:9,]))
data_test=data.frame(rbind(data[4:6,],data[10:12,]))
class.train=as.factor(c(1,1,1,2,2,2))
class.test=as.factor(c(1,1,1,2,2,2))

model  =  svm(data_train,class.train, method = "C-classification", kernel = "linear", cost = 10, scale=F)
summary(model)

pred=predict(model,data_test)
table(pred,class.test)
sum(pred==class.test)/length(class.test)

# Model building1 method = "C-classification", kernel = "sigmoid"
model  =  svm(data_train,class.train, method = "C-classification", kernel = "sigmoid", cost = 10, gamma = 0.1)
pred=predict(model,data_test)
table(pred,class.test)
sum(pred==class.test)/length(class.test)

model  =  svm(data_train,class.train, method = "C-classification", kernel = "polynomial", cost = 10, gamma = 0.1)
pred=predict(model,data_test)
table(pred,class.test)
sum(pred==class.test)/length(class.test)


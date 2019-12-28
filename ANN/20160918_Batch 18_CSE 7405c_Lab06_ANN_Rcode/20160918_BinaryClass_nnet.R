# Build Neural Network for classification using neuralnet library.
rm(list=ls(all=TRUE))

# Set the working directory
setwd("C:/Users/ISE/Desktop/20160918_Batch18_CSE7405c_Lab06_ANN")
# Importing "UniversalBank.csv" files's data into R dataframe using read.csv function.
bank_Data = read.csv(file="UniversalBank.csv", header=TRUE, sep=",")

# Understand the structure the summary of the data using str and summary R commands
str(bank_Data)
summary(bank_Data)


# Using subset remove 'ID' and 'ZIP.Code' columns from  the data
bank_Data = subset(bank_Data, select = -c(ID,ZIP.Code)) 
# Convert all the variables to appropriate type
#   To numeric using as.numeric()
#   To categoical using as.factor()
bank_Data$Education = as.factor(bank_Data$Education)


# R NN library takes only numeric attribues as input 
# Convert all categorical  attributes to numeric using appropriate technique. Hint: dummies

# Convert "Education" categorical attribute to numeric using dummy function in dummies R library
# Drop actual Education attribute from orginal data set 
# Add created dummy Education variables to orginal data set
library(dummies)
education = dummy(bank_Data$Education)
bank_Data = subset(bank_Data, select=-c(Education)) 
bank_Data = cbind(bank_Data, education)
rm(education)


# Separate Target Variable and Independent Variables.
# In this case "Personal.Loan" is a target variable and all others are independent variable. 
target_Variable = bank_Data$Personal.Loan
independent_Variables = subset(bank_Data, select = -c(Personal.Loan))

# Standardization the independent variables using decostand funcion in vegan R library
library(vegan)
# Note: To standardize the data using 'Range' method
independent_Variables = decostand(independent_Variables,"range")

bank_Data = data.frame(independent_Variables, Personal.Loan = target_Variable)
rm(independent_Variables, target_Variable)

# Convert class/Personal.Loan variable as factor
bank_Data$Personal.Loan = as.factor(bank_Data$Personal.Loan)

# Use set.seed to get same test and train data 
set.seed(123) 

# Prepare train and test data in 70:30 ratio
num_Records = nrow(bank_Data)

# to take a random sample of  70% of the records for train data 
train_Index = sample(1:num_Records, round(num_Records * 0.7, digits = 0))
train_Data = bank_Data[train_Index,] 
test_Data = bank_Data[-train_Index,] 


#Executing NN on Train Data
library(nnet)
set.seed(12634)
nn = nnet(Personal.Loan ~ ., data = train_Data,size = 8, rang = 0.1, decay = 5e-4, maxit = 500)
  
#Validating the results on test data
pred = predict(nn, test_Data, type = "class")
sort(as.numeric(unique(pred)))
conf_Matrix = table(pred, test_Data$Personal.Loan)
conf_Matrix = conf_Matrix[order(as.numeric(rownames(conf_Matrix))), ] 
#Accuracy of model
sum(diag(conf_Matrix))/sum(conf_Matrix)*100


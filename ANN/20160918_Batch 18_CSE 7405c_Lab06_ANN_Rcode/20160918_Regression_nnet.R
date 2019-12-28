# Build Neural Network for classification using neuralnet library.
rm(list=ls(all=TRUE))

# Set the working directory
setwd("C:/Users/ISE/Desktop/20160918_Batch18_CSE7405c_Lab06_ANN")
# Importing "UniversalBank.csv" files's data into R dataframe using read.csv function.
housing_data = read.csv(file="housing.csv", header=TRUE, sep=",")

# Understand the structure the summary of the data using str and summary R commands
str(housing_data)
summary(housing_data)

# Convert all the variables to appropriate type
#   To numeric using as.numeric()
#   To categoical using as.factor()

housing_data$CHAS = as.factor(housing_data$CHAS)



# R NN library takes only numeric attribues as input 
# Convert all categorical  attributes to numeric using appropriate technique. Hint: dummies

# Convert "Education" categorical attribute to numeric using dummy function in dummies R library
# Drop actual Education attribute from orginal data set 
# Add created dummy Education variables to orginal data set
library(dummies)
CHAS = dummy(housing_data$CHAS)
housing_data = subset(housing_data, select=-c(CHAS)) 
housing_data = cbind(housing_data, CHAS)


# Separate Target Variable and Independent Variables.
# In this case "OwnOcc" is a target variable and all others are independent variable. 
target_Variable = housing_data$OwnOcc
independent_Variables = subset(housing_data, select = -c(OwnOcc))

# Standardization the independent variables using decostand funcion in vegan R library
library(vegan)
# Note: To standardize the data using 'Range' method
independent_Variables = decostand(independent_Variables,"range")

housing_data = data.frame(independent_Variables, OwnOcc = target_Variable)
rm(independent_Variables, target_Variable)


# Use set.seed to get same test and train data 
set.seed(123) 

# Prepare train and test data in 70:30 ratio
num_Records = nrow(housing_data)

# to take a random sample of  70% of the records for train data 
train_Index = sample(1:num_Records, round(num_Records * 0.7, digits = 0))
train_Data = housing_data[train_Index,] 
test_Data = housing_data[-train_Index,] 


#Executing NN on Train Data
library(nnet)
set.seed(12634)
nn = nnet(OwnOcc ~ ., data = train_Data,size = 8, rang = 0.1, decay = 5e-4, maxit = 500,linout=T)
  
#Validating the results on test data
pred = predict(nn, test_Data)
sort(as.numeric(unique(pred)))

library(DMwR)
#Error verification on train data
regr.eval(test_Data$OwnOcc, pred)





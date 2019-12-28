#############################CPEE Batch16-20160430 Data Preprocessing##############################
# ###Welcome to R
# Rstudio Environment
# Editor
# Console
# Environment/History
# Files/Plots/Packages/Help/Viewer

##############################R as a Calculator####################################
2+2
2-3+1
2/3*2
2*3/2
2^3
2^3*2
2^(3*2)

###Mathematical functions in R
log(10);log(10,10) ##log(number,base)1
sqrt(100)
sin(1)
cosh(3)

# Some of the Built in constants

pi

LETTERS

letters

month.name


############################Data Types in R######################################
# a.  Numeric --real numbers 
# b.        Integer-- positive and negative whole  numbers including zero
# c.	logical-- True or False 
# d.	character-- alphabets/special characters
# e.	complex--  z<-1+2i;  Arg(0+1i); Mod(2-3i)

#Generating a sequence of numbers using scope operator and assigning to a variable
numbers<-1:15
numbers
#Generating a sequence of numbers using a "seq" function
numbers<-seq(1,10,2) 
numbers

#Using  the "c" (concatenate) Very powerful, we use this most of the time
numbers<-c(1,2,10)
numbers

#########################Variables in R########################################
# a.  Scalar -- a single number/character- Assignment can be done using "=" or "<-" or "->"
x=5
x="a" 

#b.	Vector-- a sequence of elements
x<-c(1,2,3,4,5) 
x=c("a","c")
y=c("name",7)  
#try 
A <- c(T,F,TRUE,FALSE)

z <- c("alpha",3) #c-concatenate


#To know the data type or class we type class(<variable>)

class(z)


x <- c(1,3,5,7,9)

y <- c(2,4,6,8,10)

# Element wise addition

x+y

# Elementwise subtraction

x-y

# Elementwise multiplication

x * y 

# Elementwise division

x / y

########################################

x <- c(1,2,3,4,5,6)

y <- c(10,20); 

x + y 

# here vector y gets replicated so that vector addition be completed



### Binding the vectors- Row binding and column binding

A <- rbind(x, y) 

B <- cbind(x, y)

#What if x<-c(1,2,3,4,5) and y<-c("a","b","c","d","e")

x <- c(1,2,3,4,5)

y <- c("a","b","c","d","e")

A <- rbind(x, y)  # observe the matrix data types

#c.  matrix--2d arrangement of elements (elements should be of same data type)

x <- matrix(c(1,2,3,4,5,6), nrow=3, ncol=2, byrow=T) #try with character data type

#Row sums and Column sums ,sum of diagonal elements, sum of all elements in Matrix

rowSums(x)

y <- colSums(x)

sum(diag(x))

sum(x)

## what happens if byrow=F. Compute row sums and column sums

#d.  dataframe-- it is also a matrix representation but can have multiple data types in it.
##creating an empty data frame

data <-data.frame()

#Creating a data frame

data <- data.frame( name=c("Alpha","Beta","Gamma"), Marks=c(29,NA,27)) 

#For both matrix and data frames: calling referring "elements" by position

x <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=T)

x[1,1] # extracting element in first row and first column
x[1,]  #extracting all the elements from first row
x[,2]  #extracting all the elements from second column
x[2]   #extracting thir
x[2,1] #extracting  the element in second row and first column

#To get the dimensions of the matrix

dim(x)

#To name the rows or columns in the matrix

dimnames(x)<-list(c("One","Two"),c("Three","Four"))

data <- data.frame(name=c("Alpha","Beta","Gamma"), Marks=c(29,30,27))

names(data) #  what is the output

names(data)<-c("Radiation", "Count")

#e.  list-- a  "vector" containing other objects which could be a vector, a dataframe or a list.
#Creating a list

x <- c(1,2,3,4)

y <- c("a","b")

z <-  data.frame(name=c("Alpha","Beta","Gamma"), Marks=c(29,30,27))

A <- list(x,y,z)

#Try out the following and observe the output

A[1]
A[[1]]
A[1][1]
A[[1]][1]

#Now we want to unlist them

C <- unlist(A)  # observe the output
C <- unlist(A[[3]]) # observe the output
C <- data.frame(A[[3]])# observe the output

#################Saving the work space as image, reading and loading data##################
# The workspace is your current R working environment and includes any user-defined
# objects (vectors, matrices, functions, data frames, and lists)
# The current working directory is the directory from which R will read files and to
# which it will save results by default. You can find out what the current working direc-
#   tory is by using the getwd() function. You can set the current working directory by
# using the setwd() function. If you need to input a file that isn't in the current working
# directory, use the full pathname in the call.

setwd("F:\\insofe_official\\Batch 17\\30-Apr\\RIntro")
getwd()

# you can remove objects from the workspace with rm() command


### summary Statitics
setwd("F:\\TA\\Labs\\R Basics\\20160430_Batch17_CSE7315c_Statistics_Basics\\RIntro\\RIntro\\Data")
mydata = read.csv("CustTransDat.csv")

# See the structure of the data
str(mydata)

# Look at the summary statistics
summary(mydata)

#a.	Saving workspace

save.image() / save.image("Save_20160131.RData")

# Loading saved workspace

load("Save_20160131.RData")

#b.	How do save only a few variables from environment

save(x, y, file="z.RData")

#c.	Writing data to a file

write.csv(z,"./Data/data.csv", row.names=F)

#d.	Reading the csv files and RData files into R environment

grade <- read.csv("./Data/Grade.csv", header=T, sep = ",")

##Reading other formats we use read.table command

read <- read.table("./Data/greek.txt",sep="\t",header=T)

# removing objects from the workspace

rm(x)

x  # Error: object 'x' not found

# remove all objects from the workspace

rm(list=ls(all=TRUE))


########################Data Preprocessing#################################################

#################Saving the work space as image, reading and loading data##################
#a.	Saving workspace
    save.image() / save.image("Save_20160501.RData")

#b.	How do save only a few variables from environment- 
    save(x,y,file="z.RData")

#c.	Writing data to a file
    write.csv(z,"C :/Users/ak/Documents/data.csv", row.names=F)
#d.	Reading the csv files and RData files into R environment
    grade<-read.csv("F:\\CPEE\\Batch 18\\20160604_Batch18_CSE7315c_Statistics_Basics\\RIntro\\Data\\Grade.csv",header=T,sep=",")

##Reading other formats we use read.table command
     read<-read.table("greek.txt",sep="\t",header=T)

#e. Reading Excel files  ##There might be an RJava issue
    #install.packages("XLConnect")
    require(XLConnect) #library(XLConnect) # To load an add-on package
    wb<-loadWorkbook("F:\\CPEE\\Batch 18\\20160604_Batch18_CSE7315c_Statistics_Basics\\RIntro\\Data\\Book3.xlsx")
    sheet<-readWorksheet(wb,"Sheet1",header=T)
    library(xlsx)
    write.xlsx(sheet,"F:\\CPEE\\Batch 18\\20160604_Batch18_CSE7315c_Statistics_Basics\\RIntro\\Data\\write.xlsx",row.names=F)
    
##To read the written file
   mydata <- read.xlsx("F:\\CPEE\\Batch 18\\20160604_Batch18_CSE7315c_Statistics_Basics\\RIntro\\Data\\write.xlsx", sheetName = 1)

#Every time giving the path is time consuming. so we set the working directory in which there are files  and then just use the names of files for loading
#How to now the present working directory
    getwd() ##Observe the slash
#To set the working directory
    setwd("F:\\CPEE\\Batch 18\\20160604_Batch18_CSE7315c_Statistics_Basics\\RIntro\\Data")
    


#Help in R
?merge

###################Simple manipulations of data frames in R################################
setwd("F:/CPEE/Batch 18/20160604_Batch18_CSE7315c_Statistics_Basics/RIntro/Data")
Grade1<-read.csv("Grade1.csv",header=T,sep=",")
Grade2<-read.csv("Grade2.csv",header=T,sep=",")
Names <-read.csv("Names.csv",header=T)
#To get the dimensions of the data (number of records and number of variables)
dim(Grade1)
#To get the structure and summary of the data
str(Grade1)
summary(Grade1)
head(Grade1) #outputs the first 6 records
tail(Grade1) #outputs the last 6 records

#Calling the elements using the column names
Grade1$Student.id[1] # first element in the name column
Grade1$English1[2:3] #second and third elements in the Marks column

#Referring the elements by position using the row and column indices

Grade1[,2] #all elements in second column are displayed
Grade1[1:2,3] # first and second values from the third column
Grade1[c(1,7,5),] #the first, seventh and 5th records from all columns of data

#We observed above that the structure for student.id is numeric.Does it make sense to have statistical summary of it
#When data is loaded in R, by default R assigns a data type to it. If we find it inappropriate we manually change it

Grade1$Student.id<-as.character(Grade1$Student.id) # converts into a character variable
class(Grade1$Student.id)

##Similarly if we want to convert a numeric value to factor(ordinal) we use as.factor as we have used above.
#Converting student.id to numeric again

Grade1$Student.id<-as.numeric(Grade1$Student.id)
class(Grade1$Student.id)
##We have two data sets of Grades for students. We observe that student.id is common in both
##We want to merge these two data sets into one.

mergeddata <- merge(Grade1,Grade2) ##There are other forms of merging like left join/outer join,inner join etc. 
                                ## Explore the possible joins on other data sets

MergedData<-merge(Grade1,Grade2,by.x="Student.id",by.y="Student.id",all.x=TRUE) #left (outer) join
Mergedgrade<-merge(MergedData,Names,by.x="Student.id",by.y="Student.id",all.y=TRUE) #right (outer) join

# An inner join of df1 and df2:
#   Return only the rows in which the left table have matching keys in the right table.
# An outer join of df1 and df2:
#   Returns all rows from both tables, join records from the left which have matching keys in the right table.
# A left outer join (or simply left join) of df1 and df2
#   Return all rows from the left table, and any rows with matching keys from the right table.
# A right outer join of df1 and df2
#   Return all rows from the right table, and any rows with matching keys from the left table.
#

####################################Loops and Conditionals################################
##For example I want to categorize the students into 3 bins based on the overall percentage as follows
# if pct < 40 then C, if 40 < pct < 60,B and if greater than 60 then A
##Using for loop

Grade1$class<-NA
for(i in 1:nrow(Grade1)){
  if(Grade1$OverallPct1[i]<40){
    Grade1$class[i]<-"C"
  }else{
    if(Grade1$OverallPct1[i]<60){
      Grade1$class[i]<-"B"
    }else{
      Grade1$class[i]<-"A"
    }
  }
}

###Doing the same using ifelse
Grade2$Grade<-ifelse(Grade2$OverallPct2<40,"C",ifelse(Grade2$OverallPct2<60,"B","A"))

##Again Giving multiple conditions in the if or ifelse statements
Grade1$class<-ifelse(Grade1$OverallPct1<40 | Grade1$Math1<60, "C",
                     ifelse (Grade1$OverallPct1<60 |Grade1$Math1<80,"B","A")) ##for "and" use "&"

######################################Functions############################################
#Built-in
#To calculate sum of numbers in R
v<-c(1,2,3,4,5)
sum(v) #Output/return the sum of the numbers in v
mean(v) #returns the average value of the numbers in v

##Custom functions
#To write a customized function for squaring
square<-function(x){
  return(x^2)
}
#Try out the following and observe the output
square(3)
v<-c(1,2,3,4,5)
square(v)
y<-data.frame(A=c(1,2,3,4),B=c(5,6,7,8))
square(y)

##################################Some useful functions used in data manipulations##################

# read in build datasets
library(help = "datasets")

#Apply
##
attach(mtcars)
data<-mtcars
##Want to find max value for each column
apply(data,2,min) #This generates the min values for each numeric attribute
##writing this to a data frame
A<-apply(data[,2:11],2,min)
A<-data.frame(min=apply(data[,2:11],2,min))
B<-apply(data[,2:11],2,max)
##We can find the stats for each of the variable separately

##If we want to have all the stats in a data frame we can write a customize function for this
stat<-function(x){
  "Mean"=mean(x)
  "Min"=min(x)
  "Max"=max(x)
  A<-data.frame(Min,Mean,Max)
  return(A)
}
stats<-apply(data[,2:11],2,FUN=stat) ##Observe the ouptput of apply.. it is a list
result<-do.call(rbind,stats) #execute the function call
#results <- do.call(rbind,apply(data[,2:11],2,FUN=stat))
result
#lapply
##to use apply on a vector and return a list 

lappy<-lapply(data[,2:11],mean)   ##Other variants like sapply and mapply also exist

#tapply-- gives a table wrt to a categorical attribute

tappy<-tapply(mtcars$mpg,mtcars$cyl,mean) # takes one function and gives the values and not a dataframe
tappy ##This gives out the mean mileage for each cylinder types

#########################################Subsetting###############################
##This might form an important aspect in Data analysis where we might want to work on a subset of data

##Subset on vectors
v<-c(1,2,3,4,5)
v[v>3]  #Output all elements greater than 3

##Subset on matrices and data frames
#a. Calling by cell positions

data1<-data[,2:11]
data1<-data[1:10,2:11]

#b. By using column names- two methods
data1<-data[,c("mpg","cyl")]

name<-c("mpg","cyl","disp","hp")
data1<-data[names(data)%in% name] ## %in% comes in handy for subsetting

#c. Using a subset function ##from help identify the argument to be given
data1<-subset(data,mpg>25,select=mpg:carb) #From data extracts all the records whose mpg>25 and all columns

#d. The same dataframe can be obtained in another way
data1<-data[mpg>25,]

##Multiple conditions can be given using "&" or "|"
data2<-data[mpg>25 & hp>75,]
data2<-subset(data,mpg>25 | gear==5,select=mpg:carb)

##Using which.max
data[which.max(mpg),]

##Using which.min
data[which.min(mpg),]

##Using which
data[which(data$mpg==max(data$mpg)),]
data[which(row.names(data) %in% c("Mazda RX4","Datsun 710")),]


################################Data Exploration and Data Aggregation Methods#######################
##These form an important aspect especially for data exploration, data understanding and to processing
## the data for model building
detach(mtcars)
##A data frame can have multiple datatypes in it like numeric, factor and logical.
library(plyr)
attach(baseball)
data<-baseball
str(data) ##outputs what to which type each variable belong to.
summary(data) ## gives the overall summary of the data,we observe that the stats are given for numerical
              ## attributes, if characters then class and mode are mentioned.

##Conversion of variable types if necessary
##We can consider "teams" as a factor ao that we can compare runs batted and home runs for teams
data$team<-as.factor(data$team)
str(data$team)
##We do this appropriate conversions first

##Missing Values
##To count the number of missing values
sum(is.na(data)) ##Gives the number of missing values in the data. What to do with the missing values

#option1. Omit all records with NA values
    data1<-na.omit(data)  ##it omits all the records which has atleast one NA value in it
    data2<-data[complete.cases(data),]  ##another way

#Option2. If the missing values are a few, then we can impute these missing values
library(DMwR)
data3<-centralImputation(data) #Central Imputation
sum(is.na(data3))

fillc <- data.frame(scores = c(1,2, NA, 4,5))
filled <- centralImputation(fillc)

fillc <- data.frame(grades = c('A','B','A',NA,'B'))
filled <- centralImputation(fillc)

data4<-knnImputation(data[,-c(1,4,5)],scale=T,k=5) #KNN Imputation, drop char type variables.
sum(is.na(data4))

##several aggegation functions

##Using tapply, aggregate function and ddply
##tapply function is very flexible function for summerizing a vector x. 
tapply(data$rbi,data$team,FUN= sum,na.rm=T)

#aggregate(x,by,FUN,...)
aggregate(data$rbi,by=list(data$team),FUN=sum,na.rm=T)

##ddply
names(data)
unique(data$team)
levels(data$team)

ddply(data,.(team),summarize, runs=sum(rbi,na.rm=T))

bdata <- data
##Please use R resources for exploring the above functions


##Randomly split  the data into two
rows<-seq(1,nrow(data),1)
set.seed(1234)
trainrows<-sample(rows,0.7*nrow(data))
Train<-data[trainrows,]
Test<-data[-trainrows,]
##By using a package caTools
require(caTools)
set.seed(123) 
sample = sample.split(data$team, SplitRatio = .70)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)



##############Working with the data ##########################
##There are several steps that we would follow for data preprocessing steps. These are not exhaustive 
##but according to the need we may use only some of these or sometimes we need to do a bit extra processing

##Understanding the data variables-- what are their types
##Data type conversions, if while loading the data the type taken is not appropriate
##Looking at the missing values  --either removing or imputing them
### Descriptive stats for distribution of data and for outlier detection
## Standardizing the data-- why scaling is important
   #a. Using Standardization
   #b. Using range
## Converting the variables 
   #From Categorical to numeric  --Dummy
   #From Numeric to categorical  -- Dicretizing (Equal Width, Equal Freq), Manual Coding

data <- read.csv(file="F:\\CPEE\\Batch 18\\20160604_Batch18_CSE7315c_Statistics_Basics\\RIntro\\Data\\dataMerged.csv",header=TRUE, sep=",")

#Undertanding data structure
str(data)
head(data)
tail(data)
edit(data)
names(data)
summary(data)

#Missing values handling

sum(is.na(data))#To check null values in data

#Dropping the recrods with missing values
data2<-na.omit(data)
rm(data2)
#To identify rows where more than 20% attributes are missing
library(DMwR)
length(manyNAs(data, 0.2) )
data[74,]

#type conversions
data$family <- as.factor(data$family)
data$edu <- as.factor(data$edu)
data$cc <- as.factor(data$cc)
data$cd <- as.factor(data$cd)
data$securities <- as.factor(data$securities)
data$online <- as.factor(data$online)
data$loan <- as.factor(data$loan)
str(data)
library(DMwR)
data2<-centralImputation(data) #Cenral Imputation
sum(is.na(data2))
data3<-knnImputation(data,scale=T,k=5) #KNN Imputation
sum(is.na(data3))
write.csv(data2, "data_imputed.csv", row.names=FALSE)

#Standardizing the data
#Subsetting data
Data_NumAtr<-subset(data2,select=c(age,exp,inc,mortgage,ccAvg))
Data_CatAtr<-subset(data2,select=-c(age,exp,inc,mortgage,ccAvg))
library(vegan)
#Using range method
dataStd. <- decostand(Data_NumAtr,"range") 
summary(dataStd.)
#Using Z score method
dataStd. <- decostand(Data_NumAtr,"standardize")
summary(dataStd.)

#Discretizing the variable
summary(data2)
library(infotheo)
IncomeBin <- discretize(data2$inc, disc="equalfreq",nbins=4)
table(IncomeBin)
#tapply usage
tapply(data2$inc,IncomeBin,min)
tapply(data2$inc,IncomeBin,max)

IncomeBin <- discretize(data2$inc, disc="equalwidth",nbins=4)
table(IncomeBin)
#tapply usage
tapply(data2$inc,IncomeBin,min)
tapply(data2$inc,IncomeBin,max)

#Manual recoding 
summary(data2$age)
data2$ageNew<-0
for (i in 1:nrow(data)){
  if (data2$age[i]>=45){ 
    data2$ageNew[i]=2
  }
  else {
    data2$ageNew[i]=1
  }
}
table(data2$ageNew)
tapply(data2$age,data2$ageNew,min)
tapply(data2$age,data2$ageNew,max)

#Creating dummy variables and adding to original table
library(dummies)
EduDummyVars<-dummy(data2$edu)
head(EduDummyVars)
data<-data.frame(data,EduDummyVars)
head(data)


# identify outlier in the data
stocks = read.csv("stocks.csv", header=T)
names(stocks)
str(stocks)
summary(stocks$Price1)

boxplot(stocks$Price1)
abline(h = mean(stocks$Price1), lty=2)
IQ = IQR(stocks$Price1)
Q1 = quantile(stocks$Price1,0.25)
Q3 = quantile(stocks$Price1,0.75)
lowerinnerfence =  Q1 - 1.5*IQ
upperinnerfence = Q3 + 1.5*IQ
lowerouterfence = Q1 - 3*IQ
upperouterfence = Q3 + 3*IQ

outliers = boxplot(stocks$Price1, plot=FALSE)$out
#Extract the outliers from the original data frame
stocks[stocks$Price1 %in% outliers,]
outlier = stocks[stocks$Price1>539,]


--------------------------------------------------------------------------------------------
  #Split data for model building and testing the predictions.
  
  smp_size <- floor(0.7 * nrow(data[]))
set.seed(2)
tr_data <- sample(seq_len(nrow(data[])), size = smp_size)
train <- data[tr_data, ]
test <- data[-tr_data, ] 





#####################################Reshape############################
##Load in the CustTransDat.csv and Grade1.csv
Cust<-read.csv("CustTransDat.csv",header=T,sep=",")
##Sometimes it may be useful to change the data the way it looks. Forexample we have an 
## a transaction data of customers for a store. We would like to know the revenue generated 
##by year and by quarter. Then reshaphing would give a good representation
names(Cust)
library(reshape2)
data2<-dcast(Cust,Quarter~Year,fun.aggregate = sum,value.var="Cost")
data2

##Another Example
meltdata<-melt(data=Grade1,id="Student.id")
head(meltdata)

#aggregating the data based on subject and gender. This is also called the wide format.
data2<-dcast(data=meltdata,Student.id~variable,value="value") 
data3<-dcast(data=meltdata,variable~Student.id,value="value",fill=0) #Observe the difference in data2 and data3

###Resources for Reshape- http://seananderson.ca/2013/10/19/reshape.html


######################Regular expression##############################
symbols =read.csv("Symbols.csv", header =T)
names(symbols)
summary(symbols)
str(symbols)
symbols$salespct=gsub("[%]","",symbols$salespct)
symbols$salespct =as.numeric(symbols$salespct)

A<-data[grep("gerhajo01",data$id),]  ## gives out the indices where it is there
A<-data[grepl("gerhajo01",data$id),] ##gives logical output if it is there or not


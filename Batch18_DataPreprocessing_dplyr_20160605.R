rm(list = ls(all=T))

#install.packages("downloader")
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)
dim(msleep)

################***dplyr***###############
# install.packages("dplyr")
library(dplyr)

### Main functions in dplyr
# select()	select columns
# filter()	filter rows
# arrange()	re-order or arrange rows
# mutate()	create new columns
# summarise()	summarise values
# group_by()	allows for group operations in the "split-apply-combine" concept

### select
# Select specific columns
sleepData <- select(msleep, name, sleep_total)
head(sleepData)
# Select all, except specific solumns
head(select(msleep, -name))
# Look at the column names
names(msleep)
# Select a series of columns
head(select(msleep, name:order))
# Select columns starting with specific names
head(select(msleep, starts_with("sl")))

### filter
# Select rows w.r.t a particular column values
filter(msleep, sleep_total >= 16)
# Select rows which satifty multiple conditions
filter(msleep, sleep_total >= 16, bodywt >= 1)
# Select rows, according to specific values of an attribute
filter(msleep, order %in% c("Perissodactyla", "Primates"))

### arrange
# Arrange the rows according to specific order of an attribute
arrange(msleep, sleep_total)
arrange(msleep, -sleep_total)
arrange(msleep, order, sleep_total)
arrange(msleep, desc(order), sleep_total)

### mutate
# Create a new column using other columns
mutate(msleep, rem_proportion = sleep_rem / sleep_total)
mutate(msleep, rem_proportion = sleep_rem / sleep_total, 
       bodywt_grams = bodywt * 1000)

### summarise
# Find summary of columns
names(msleep)
summarise(msleep, avg_sleep = mean(sleep_total))

summarise(msleep, 
          avg_sleep = mean(sleep_total), 
          min_sleep = min(sleep_total),
          max_sleep = max(sleep_total),
          total = n())

### Group_by
# Summarize data by levels in an attribute
a = summarise(group_by(msleep, order), 
          avg_sleep = mean(sleep_total), 
          min_sleep = min(sleep_total), 
          max_sleep = max(sleep_total),
          total = n())
### Chaining
# Call multiple functions in a single go
msleep %>%
  select(name, sleep_total) %>%
  head

msleep %>%
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>%
  head

msleep %>%
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total),
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())
################***substr***###############
# Generate a random data frame
data = data.frame(id=1:10,
                  Age=rnorm(10, 40, 10),
                  Income=rnorm(10, 50000, 20000),
                  Place=c("Bang_KT", "Belgaum_KT", "Pune_MH", "Mumbai_MH", "Hyd_TS", "Chennai_TN", "Coimbatore_TN",
                          "Vellore_TN", "Jaipur_RJ", "kolkata_WB"),
                  Occupation=sample(c("Engineer", "Doctor", "Lawyer", "Scientist", "Journalist"), 10, replace = T))

data
str(data)
data$Place <- as.character(data$Place)

library(dplyr)
#attach(data)
data = mutate(data, City = substr(Place, 1, nchar(Place)-3))
data = mutate(data, State = substr(Place, nchar(Place)-1, nchar(Place)))
print(data)

#################***table***################
a = sample(c(1,2,3,4), 10, replace = T)
a
b = as.data.frame(table(a))
b
b[which(b$Freq == max(b$Freq)),]
attach(b)
b[which.max(Freq), ]

################***set.seed***###############
# sample(c(1,2,3,4,5), 10, replace = T)
# sample(c(1,2,3,4,5), 10, replace = T)
# sample(c(1,2,3,4,5), 10, replace = T)
# 
# # Setting the same seed will create the same random numbers every time
# set.seed(572)
# sample(c(1,2,3,4,5), 10, replace = T)
# sample(c(1,2,3,4,5), 10, replace = T)
# sample(c(1,2,3,4,5), 10, replace = T)
# 

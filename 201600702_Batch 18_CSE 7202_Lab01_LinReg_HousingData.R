rm(list=ls())
setwd("F:\\CPEE\\Batch 18\\CSE 7202c\\Linear Regression")
## data reading and descriptives
HousData <- read.csv("Housing.csv", header=T)
names(HousData)
str(HousData)
summary(HousData)
dim(HousData)

## type conversion
HousData$CHAS = as.factor(HousData$CHAS)

## visualizing relationships 

h<-hist(HousData$MEDV, main = "distribution of median prices of houses", col="green")
x<- HousData$MEDV
xfit<-seq(min(x),max(x),length=60) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

d<- density(HousData$MEDV)
plot(d)
polygon(d, col="red", border="blue")


par(mfrow=c(2,2))
plot(HousData$CRIM, HousData$MEDV, col = "red", type ="p", main = "Relation between Crime and median housing prices", xlab= "Crime rate", ylab = "Median housing prices", pch=19)
plot(HousData$INDUS, HousData$MEDV, col = "blue", type ="p", main = "Relation between INDUS and median housing prices", xlab= "INDUS", ylab = "Median housing prices", pch=21)
plot(HousData$LSTAT, HousData$MEDV, col = "green", type ="p", main = "Relation between LSTAT and median housing prices", xlab= "LSTAT", ylab = "Median housing prices", pch=22)
plot(HousData$B, HousData$MEDV, col = "black", type ="p", main = "Relation between B and median housing prices", xlab= "B", ylab = "Median housing prices", pch=24)


##  the lower right quadrant, we see that, unsurprisingly, the more lower
#  economic status residents a neighborhood has, the lower the median house value. From the upper
#  right and lower left corners we see (again, unsurprisingly) that higher crime rates are associated with
#  lower median values

# All the very high crime rates seem to be associated with a specific, mid-range value of INDUS (proportion of non-ret
# businesses per neighborhood). That a specific, middling level of INDUS is really associated with
# high crime rates seems dubious

plot(HousData)
dev.off()

## correlations between numeric variables
IndCor <- cor(HousData[,-4])
IndCor

library(corrplot)
corrplot(IndCor, method="circle")  
#method = "square" | "ellipse" | "number" | "shade" | "pie"
#type = "upper" | "lower"
#order = "alphabet" | "hclust"
# and more...https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html


## box plots
boxplot(HousData$MEDV)
boxplot(HousData$MEDV~HousData$CHAS, 
        col=(c("gold","darkgreen")),
        main="Tract bounds and Median house prices", xlab="Tract bounds")

boxplot.stats(HousData$MEDV)
outliers = boxplot(HousData$MEDV)$out
HousData[HousData$MEDV %in% outliers,]

?boxplot.stats

## simple linear relationship visual
plot(HousData$LSTAT, HousData$MEDV, col = "green", type ="p", main = "Relation between LSTAT and median housing prices", xlab= "LSTAT", ylab = "Median housing prices", pch=22)
abline(lm(HousData$MEDV~HousData$LSTAT))

library(scatterplot3d)
scatplot <- with(HousData, {
  scatterplot3d(HousData$MEDV,   # x axis
                HousData$CRIM,     # y axis
                HousData$INDUS,    # z axis
                main="3-D Scatterplot",pch=19, color ="green",
  xlab="Median Housing price",
  ylab="Crime rate",
  zlab="Prop. Non-retail business"
  ) })

## Split the data into Train and Test sets
smp_size <- floor(0.7 * nrow(HousData))
set.seed(123)
tr_data <- sample(seq_len(nrow(HousData)), size = smp_size)
train <- HousData[tr_data, ]
test <- HousData[-tr_data, ] 


# Assumptions  of linear regression
# The residual errors are normally distributed
# Variance of the residuals is constant
# Different variables are linearly independent

# Ho: there is no relationship between the independent and dependent variable
# H1: atleast one independent variable has relationship with dependent variable

## simple linear regression
SimLm <- lm(MEDV~CRIM, data=train)
summary(SimLm)

coef(SimLm)  #coefficients of the model
resid(SimLm) # residuals of the model fit
fitted(SimLm) # predicted values 

boxplot(resid(SimLm))
par(mfrow=c(2,2))
plot(SimLm)

## equation = MEDV = 24.2072477 - 0.4082702 * CRIM

plot(HousData$CRIM, HousData$MEDV, col = "red", type ="p", main = "Relation between Crime and median housing prices", xlab= "Crime rate", ylab = "Median housing prices", pch=19)
abline(SimLm, col="blue")

#From the output What should one look for to understand if the model is good

# CRIM is significant with 95% confidence
# R-square is very low but p value is less than 0.05 indicating 
# F : Between var/Within var.
# rejecting null hypothesis

## error on train data

library(DMwR)
regr.eval(train$MEDV,fitted(SimLm))
pred <- predict(SimLm, test)
pred
regr.eval(test$MEDV, pred)


# plot 1
# The residuals should be randomly distributed around the horizontal line representing a
# residual error of zero; that is, there should not be a distinct trend in the distribution of
# points.

#plot2
# standard Q-Q plot, which should suggest that the
# residual errors are normally distributed

#plot3
# The scale-location plot in the upper right shows
# the square root of the standardized residuals (sort of a square root of relative error) as a
# function of the fitted values. Again, there should be no obvious trend in this plot. 

# plot 4
# the plot in the lower right shows each points leverage, which is a measure of its
# importance in determining the regression result.Smaller distances means that removing the observation has
# little affect on the regression results. Distances larger than 1 are suspicious and suggest
# the presence of a possible outlier or a poor model

## Multiple linear regression

MultLM <- lm(train$MEDV~., data=train)
summary(MultLM)

boxplot(resid(MultLM))

library(DMwR)
# error
regr.eval(train$MEDV,fitted(MultLM))  #error on train data
pred <- predict.lm(MultLM, test) 
regr.eval(test$MEDV, pred)#error on test data


## variable selection using stepwise regression to find the best model
library(MASS)
stepLM <- stepAIC(MultLM, direction="both")
stepLM$anova # display results

train1 <- train[,-c(3,7)]
test1 <- test[,-c(3,7)]
BestLM <- lm(train1$MEDV~., data=train1)
summary(BestLM)

# error
regr.eval(train1$MEDV,fitted(BestLM))  #error on train data
pred1 <- predict.lm(BestLM, test1) 
regr.eval(test1$MEDV, pred1)#error on test data


## VIF   sqrt(vif(fit))> 2 then remove the variable
library(car)
vif(MultLM) 
vif(BestLM)

## diagnostics
par(mfrow=c(2,2))
plot(BestLM)

#outlierTest(BestLM)
#leveragePlots(BestLM)

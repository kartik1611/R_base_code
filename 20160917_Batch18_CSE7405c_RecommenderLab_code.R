#https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
rm(list=ls(all=TRUE))

#install.packages("recommenderlab")
#install.packages("lsa")

library(recommenderlab)
library(lsa)

#simulating a user-item rating matrix
set.seed(5643)
m <- matrix(sample(c(as.numeric(0:5), NA), 50,
                   replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
            dimnames=list(user=paste("u", 1:5, sep=""),
                          item=paste("i", 1:10, sep="")))
m

#converting it into a realRatingMatrix
r <- as(m, "realRatingMatrix")
r

#identical(as(r, "matrix"),m)

#Normalization
r_m <- normalize(r)
r_m
(as(r_m, "data.frame"))

#Small portions of rating matrices can be visually inspected using image().
image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")
r_b <- binarize(r, minRating=4)
image(r_b)

#creating recommender
r1 <- Recommender(r, method = "UBCF")
r1

#The model can be obtained from a recommender using getModel().
#names(getModel(r1))

recom <- predict(r1, r[1], n=5)
recom
as(recom, "list")

# #Lets check how we got them:
# #our user of interest if u3, and we wNT TOP 4 recommendations
# 
# #find the users who are similar to u3
# cosine(t(as(r@data[1:5,],"matrix"))) #its user 4
# 
# #check the items user has rated and user3 has n't rated yet and recommend accordingly
# m

# recommendation
recom1 <- predict(r1, r[4], type="ratings")
recom1
as(recom1, "matrix")

#split thedata into train and evaluation sets
e <- evaluationScheme(r, method="split", train=0.8,
                      given=3, goodRating=3)

# We create two recommenders (user-based and item-based collaborative filtering) using the
# training data.
r2 <- Recommender(getData(e, "train"), "UBCF")
r2

r3 <- Recommender(getData(e, "train"), "IBCF")
r3

# Next, we compute predicted ratings for the known part of 
#the test data (3 items for each user) using the two algorithms.

p1 <- predict(r2, getData(e, "known"), type="ratings")
p1
as(p1, "list")
as(p1, "matrix")

p2 <- predict(r3, getData(e, "known"), type="ratings")
p2
as(p2, "list")
as(p2, "matrix")

# Finally, we can calculate the error between the prediction and the unknown part of the test
# data.

error <- rbind(
  calcPredictionAccuracy(p1, getData(e, "unknown")),
  calcPredictionAccuracy(p2, getData(e, "unknown"))
)
rownames(error) <- c("UBCF","IBCF")
error

#In this example user-based collaborative filtering produces a smaller prediction error.

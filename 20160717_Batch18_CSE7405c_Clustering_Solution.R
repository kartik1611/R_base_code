# Removing all the variables from the workspace
rm(list = ls(all.names = T))
# Attching the data
attach(mtcars)
MyData <- mtcars
str(MyData)
# Checking for the NA values
sum(is.na(MyData))

# Loading the required packages
library(DMwR)
library(vegan)

# Normalizing the data
MyData <- decostand(MyData,method = "range")
# Calculating the distance bases on "euclidean" distance
d <- dist(MyData,method = "euclidean")
d
# Clustering the data based on hirearichal clustering
fit <- hclust(d, method = "ward")
# Plot dendogram
plot(fit)
# Limiting the cluster to 5
groups <- cutree(fit,k=5)
groups
# Viewing the 5 cluster highlighted in red
rect.hclust(fit,k=5,border = "red")

# Clustering the data using k-means and consider 5 clusters
fit <- kmeans(MyData,centers = 5)
# Calculating the sum of withiness of the error for all the clusters
sum(fit$withinss)

# K-means:  Determine number of clusters

wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(MyData,centers=i)$withinss)
}

# Plot the cluster number and withinness error
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

##################################################
setwd("D:\\Gautham PC\\D\\Insofe\\Batch 18\\20160717_Batch18_Clustering and PCA_LabActivity\\")
# Reding the data
CerealsData<- read.csv('Cereals.csv',header = T)
summary(CerealsData)

# Ignoring the dependent attribute
CerealsDt <- as.matrix(CerealsData[,-c(1)])
rownames(CerealsDt) <- CerealsData$name

# Cheking for number or NA values in data
sum(is.na(CerealsDt))
# library(DMwR)
CerealsDt <- knnImputation(CerealsDt,k = 5)
# Scale the attributes
CerealsDt <- decostand(CerealsDt,method = "range")

# Calculate the euclidean distance
d <- dist(CerealsDt,method = "euclidean")
d

# Ward Hierarchical Clustering
fit <- hclust(d, method = "ward")
# display dendogram
plot(fit)
# Cut the tree to 5 custers
groups <- cutree(fit,k=5)
groups
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit,k=5,border = "red")
# Custering the data using K-means, creating 5 clusters
fit <- kmeans(CerealsDt,centers = 5)
# Getting the withiness of the error of all the clusters
sum(fit$withinss)
fit$centers
fit$cluster

# K-means:  Determine number of clusters

wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(MyData,centers=i)$withinss)
}

# Plot the cluster number and withinness error
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
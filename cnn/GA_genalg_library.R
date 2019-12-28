#http://www.r-bloggers.com/genetic-algorithms-a-simple-r-example/

rm(list=ls(all=TRUE))

library(genalg)

dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "onions", 
                               "sleeping bag", "rope", "compass"), survivalpoints = c(10, 20, 15, 2, 30, 
                                                                                      10, 30), weight = c(1, 5, 10, 1, 7, 5, 1))
weightlimit <- 20

chromosome = c(1, 0, 0, 1, 1, 0, 0)
dataset[chromosome == 1, ]

#We can check to what amount of surivival points this configuration sums up.

cat(chromosome %*% dataset$survivalpoints)
## 42

#We define the evaluation function as follows.

evalFunc <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) else return(-current_solution_survivalpoints)
}

iter = 100
#size : the number of genes in the chromosome
GAmodel <- rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)
plot(GAmodel)
cat(summary(GAmodel))

solution = c(1, 1, 0, 1, 1, 1, 1)
dataset[solution == 1, ]
#This in turn gives us the total number of survival points.

# solution vs available
cat(paste(solution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints)))
## 102 / 117
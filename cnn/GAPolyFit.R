rm(list=ls(all=TRUE))
setwd("C:/Users/Admin/Desktop/Sims/")

#Find polynomial coefficients

p=runif(100,1,10)
q=runif(100,1,10)
r=runif(100,1,10)
s=runif(100,1,10)

p=runif(500,1,10)
q=runif(500,1,10)
r=runif(500,1,10)
s=runif(500,1,10)

initPop=cbind(p,q,r,s)

eval<-function(individual){
  
  dataset <- data.frame(x = c(0, 1, 2, 3), 
                        y = c(4, 14, 42, 100))
  attach(dataset)
  fitness=sum((y-((individual[1]*x^3)+
                    (individual[2]*x^2)+
                    (individual[3]*x)+
                    individual[4]))^2)
  detach(dataset)
  return(fitness)
}

mutate<-function(individual){
  #Interchange the values of two of the attributes
  a=sample(1:4,2)
  k=individual[a[1]]
  individual[a[1]]=individual[a[2]]
  individual[a[2]]=k
  return(individual)
}

mutate<-function(individual){
  #change one value to 10-that value
  
  a=sample(1:4,1)
  individual[a]=1/individual[a]
  return(individual)
}

crossOver=function(p1,p2){
  pos=sample(1:3,1)
  nxt=pos+1
  bred=c(p1[1:pos],p2[nxt:4])
  return(bred)
}

crossOver=function(p1,p2){
  wt=runif(1,0,1)
  bred=wt*p1 +(1-wt)*p2
  return(bred)
}

geneticAlgo<- function(population,fitnessFun,
                       mutate, 
                       crossOver, mutProb,elite, 
                       maxiterations){
  
  cat("max iterations =", maxiterations, "\n")
  # How many winners from each generation?
  
  origPopSize=nrow(population)
  topElite=round(elite*origPopSize,0)
  fitN=apply(population,1,fitnessFun)
  population=data.frame(population,fitN)
  population=population[sort.list(population[,5]), ]
  # Main loop
  
  for (i in 1:maxiterations) {
    population=population[1:topElite,]
    population=population[,-c(5)]
    mut=mutProb/i
    
    # Add mutated and bred forms of the winners
    while (nrow(population)<origPopSize) {
      # Mutation
      if (runif(1,0,1)<=mut) {
        c=sample(1:topElite,1)
        population[nrow(population)+1,]=mutate(population[c,])
      }
      
      # Crossover
      else {
        c1=sample(1:topElite,1)
        c2=sample(1:topElite,1)
        population[nrow(population)+1,]=data.frame(crossOver(population[c1,], population[c2,]))
      }
      
    }
    
    population[,5]=apply(population,1,fitnessFun)
    population=population[sort.list(population[,5]), ]
    
    # Print current best score
    
    cat("best fitness score in iteration", i, "=", population[1,5], "\n")
    
    if (population[1,5]==0){
      return(population[1,])
      i=maxiterations
    }
    #returns the best solution   
    
  }
  return(population[1,])
}

geneticAlgo(initPop, eval, mutate, 
            crossOver,0.25,0.1,10)
geneticAlgo(initPop, eval, mutate, 
            crossOver,0.05,0.1,10)
geneticAlgo(initPop, eval, mutate, crossOver,0.005,0.1,10)
geneticAlgo(initPop, eval, mutate, crossOver,0.05,0.2,10)
geneticAlgo(initPop, eval, mutate, crossOver,0.05,0.3,20)

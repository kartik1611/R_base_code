rm(list=ls(all=TRUE))
#Project management and learning R programming;  

#What is the most 
#likely time to complete the project given 
#the following conditions

#Total modules: 10 to 12 if we get part of the job
#Total modules: 20 to 24 if we get the whole job
#There is a 10% chance that we get full project or else part job 
#Tasks per module: 50 to 100  
#People per module: 3:7
#Time per task: 5 to 10 days

#Let us conduct 10, 100, 1000, 10000 simulations.  Then compute the average to estimate

#The function below is our own first R function.  
#It takes as input number of simulations and computes the timeNeeded

timeNeeded= function(numSims) {
  time=0
  set.seed(12445)
  for (i in 1:numSims) {
    
    partOrFull=runif(1,0,1)
    # 90-10 probability for full and part 
    if (partOrFull<=0.1){
      # part project is possible as per probability
      totalModules=sample(20:24, 1)
    }else{ 
      # full project is possible as per probability
      totalModules=sample(10:12, 1)
    }
    
    #Two vectors are created randomly to represent tasks and people per module
    
    tasksPerModule=sample(50:100, totalModules,replace=T)
    totalTasks=sum(tasksPerModule)
    #Compute the time for all tasks.  Each task can take anywhere between 5 to 10 hours
    timeToDoTasks=sum(sample(5:10,totalTasks,replace=TRUE))
    # Resources for a project
    Resourcesperproject=sum(sample(3:7, 1)) 
    #Time needed according to this simulation
    
    time[i]=timeToDoTasks/Resourcesperproject
  }
  
  
  #The time vector is returned
  cat("Time Needed based on", numSims, "simulations= ", mean(time), "\n")
  return(mean(time))
}

simulations=c(10,100,1000,10000)

start = Sys.time()
for (i in simulations) {
  Time=timeNeeded(i)
  #print(Time)
  #cat("Time Needed based on", i, "simulations= ", Time, "\n")
  
}
end=Sys.time()-start
end

# instead for loop use sapply
start1 = Sys.time()
time1 = sapply(simulations,timeNeeded)
end1=Sys.time()-start1
end1

start2 = Sys.time()
time2 = lapply(simulations,timeNeeded)
end2=Sys.time()-start2
end2


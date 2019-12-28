
rm(list=ls(all=TRUE))

dataset = data.frame(Stock = c("CA","CTAG","DS","EM","EQMN","ED"),
                     Returns = c(0.0119,0.0393,0.0178,0.0791,0.0189,0.0213))
MinInvestment = 0.03

Amount = 150000
StoppingCriteria = 8000
weightlimit <- 1
1/6
fnInitialsolu <- function(){
  # Let us initiate solution with almost equal weights to make it total 1
  Initialsolu =c(0.16,0.17,0.16,0.17,0.17,0.17)
  
  return(Initialsolu)
}

#We define the evaluation function as follows.

fnEvaluate <- function(x,Amount) {
  
  IndividualInvestments <- (x * rep(Amount,length(x)))
  TotalReturns = dataset$Returns %*% IndividualInvestments   # multiply and sum            
  
  if (round(sum(x),1) != weightlimit) 
    return(0) else return(TotalReturns)
}


# Purterbation : randomly select a point and do operation
fnPurterbSA=function(individual){
  a=sample(1:length(individual),2)
  individual1 = individual
  x = individual[a[1]]
  y = individual[a[2]]
  # taking the diff of 1st stock weight and 0.03 and take half of it 
  # subtract that value and the same value to the secnd stock weight ( as total should be 1)
  diff = ((individual[a[1]]-0.03)/2)
  individual1[a[1]]=individual[a[1]]-diff
  individual1[a[2]]=individual[a[2]]+diff
  return(individual1)
}



fnRunSimulatedAnnealingAlgo <- function( maxiterations,Amount){
  
  
  cat("max iterations =", maxiterations, "\n")
  
  weightlimit = 1
  # Generate a random solution
  initialsolu = fnInitialsolu()
  initialval = fnEvaluate(initialsolu,Amount)
  
  basesolu = initialsolu
  baseval = initialval
  counter = 0
  # solution vs available
  cat(paste("baseval initially is : ", baseval,"\n"))
  
  for (i in 1:maxiterations) {
    
    
    # Purterbation
    
    nextsolu = (fnPurterbSA(basesolu))
    nextval = fnEvaluate(nextsolu,Amount)
    
    #     print("nextval")
    #     print(nextval)
    #     print("nextsolu")
    #     print(nextsolu)
    #     
    if(any(nextsolu > 0.03)==FALSE){
      return(0)
    }else{
      counter = counter+1
      if(nextval > baseval){
        basesolu = nextsolu
        baseval = nextval
      }else{
        # accept with acceptnce probability
        Acceptanceprob = runif(1, 0, 1)
        if(Acceptanceprob > 0.5){
          basesolu = nextsolu
          baseval = nextval
        }
        
      }
      
      
    }
    if (baseval >= StoppingCriteria){break()}     
    i = counter
    
    # solution 
    cat("baseval in ", " iteration ", i, "is : ", baseval,"\n")
    
  }
  
  
  return(list(basesolu,baseval)) 
}



fnExecuteMain <- function(dataset, maxiterations,Amount){
  
  set.seed(1234)
  
  solutionlist = fnRunSimulatedAnnealingAlgo( maxiterations,Amount)
  
  Finalsolution = as.numeric(solutionlist[[1]])
  Finalsolutionvalue = solutionlist[[2]]
  dataset$Finalsolution = Finalsolution
  
  cat(" Total Returns = ",Finalsolutionvalue,"\n")
  return(dataset)
  
}

maxiterations=1000
Amount = 150000
Result = fnExecuteMain(dataset, maxiterations,Amount)



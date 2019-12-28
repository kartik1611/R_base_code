rm(list=ls(all=TRUE))
par(mfrow=c(1,1))

#How do random variables behave when summed or multiplied
x <- rnorm(1000,100,5)
y <- rnorm(1000,100,5)
z <- rnorm(1000,100,5)
plot(dnorm(x,100,5)~x)
a <- x+y+z
plot(dnorm(a,300,5)~a)
mean(a)
sd(a)

x <- rnorm(1000,10,2)
y <- rnorm(1000,10,2)
z <- rnorm(1000,10,2)

b <- x*y*z
plot(dnorm(b,mean(b),sd(b))~b)
mean(b)
sd(b)

#Monte Carlo Simulations
# Computing Pi 

#Let us define a vector that carries the 
#number of simulations we want to conduct.  As we want to see how the value converges, we shall 
#Conduct simulations where in each 
#simulation, we throw 10 or 100 or 
#1000 or 10000 or a million throws.  

sims <- c(10,100,1000,100000,1000000) 

#Here, you learn a key concept in R programming namely a for loop.  It 
#creates a temporary variable called I and starts with the first value 
#in simulations (10).  Execute the entire code within the curly 
#brackets and then increments the value of "i" to next value in sims.  
#It stops when it executed the code for all values of 

for (i in sims) {
  
  #Defined two variables to keep the counts of those darts that fell 
  #within the circle and outside the circle (but within the square)
  
  within <- 0
  
  #The landing point of the dart can be explained by its x and y coordinates. 
  #We assume our circle to have a radius of 1 unit and center is origin.  
  #Then, the coordinates of the sides will be (0,0), (1,0), (0,1), (1,1).  
  #So, we simulate random landing of the dart by simulating random x and 
  #y coordinates with a restriction that coordinate falls within 0 and 1.
  
  x=runif(i,0,1)
  y=runif(i,0,1)
  
  #This is loop within loop.  Our goal here is to see if the dart fell 
  #within the circle or outside and update our within and outdate accordingly.
  
  for (j in 1:i) {
    
    #The following is another very important construct in R.  It is a 
    #conditional statement.  Essentially, if the conditional is true, the 
    #statement(s) following it will be executed.  Else, the statements 
    #following "else" will be executed.  We measure the distance between 
    #the point and the origin.  If the distance is less than the radius, 
    #then that dart fell within the circle.  Else, it fell outside the 
    #circle.
    
    if((x[j]^2+y[j]^2)<=1) 
      within=within+1
  }
  
  #As agreed, pi is 4 times the ratios of darts as these are proportional 
  #to the areas.
  
  pi=4*within/i
  
  #cat is the command to print the output on the screen
  
  cat("For", i, "simulations, pi =", pi, "\n")
}

#A better way to write the same program.  Avoid loops as much as 
#possible in R

#5 million simulations the previous way

within=outSide=0  
x=runif(5000000,0,1)
y=runif(5000000,0,1)

start_time=proc.time()
for (j in 1:5000000) {
  
  if((x[j]^2+y[j]^2)<=1) within=within+1
}

pi=4*within/5000000
exe_time=proc.time() - start_time
exe_time
pi

#5 million simulations without loops

start_time=proc.time()
pi=4*sum(x^2+y^2<=1)/5000000
exe_time=proc.time() - start_time
exe_time
pi

#We are defining a function called rolls which takes the possible options in A, 
#in B and number of simulations.  Functions are extremely powerful blocks of 
#code that can makes programming efficient.

rm(list=ls(all=TRUE))

rolls= function(possibA,
                possibB,sims){
  
  i=0
  
  #A loop is needed to throw as many times as determined by simulations
  
  for(j in 1:sims){
    
    #Simulating a simultaneous throw.
    
    Aval=sample(1:possibA,1)
    Bval=sample(1:possibB,1)
    
    #If first dice gives higher value, the count is updated.
    
    if (Aval>Bval){i=i+1}
    
  }
  
  result=i/sims
  
  #Function returns this value that return specifies.  Or function returns the last #value computed.
  
  return(result)
}

rolls(6,8,50000)


#Capital Market Modeling

##Let us define the returns matrix and initialize the investments at Rs. 1000
#As we do 1000 simulations again, we store them in arrays of same length

rm(list=ls(all=TRUE))
Returns=matrix(c(0.8, 0.06, 0.9, 0.9,
                 0.2, 1, 1.05, 1, 1, 
                 1.1, 3, 1, 1.2, 3, 
                 1, 1.4, 3, 1.1), 
               nrow=6, ncol=3, byrow=TRUE)
Returns
Green=Red=Blue=1000

GreenOutcome=RedOutcome=
  BlueOutcome=rep(0, 10000)

##We are preparing to conduct 10000 iterations

sims = 1

while (sims <= 10000) {
  
  ##We are dealing with a different looping construct to do 1000 simulations.  
  #Each iteration tests the returns of each stock after 20 years
  
  for (i in 1:20) {
    
    ##The exact outcome is modeled to be random
    
    Outcome = sample(1:6,1)
    
    Green=Returns[Outcome,1]*
      Green
    Red=Returns[Outcome,2]*Red
    Blue=Returns[Outcome,3]*Blue
    
  }
  
  ##Let us store the results of the jth iteration as jth value in each array
  
  GreenOutcome[sims]=Green
  RedOutcome[sims]=Red
  BlueOutcome[sims]=Blue
  
  ##Let us reinitialize and start a new iteration
  
  Green=Red=Blue=1000
  sims=sims+1
}

##We consider median and not mean because of the outliers
##Why?

AverageGreen=median(GreenOutcome)
AverageRed=median(RedOutcome)
AverageBlue=median(BlueOutcome)

barplot(c(AverageGreen,AverageRed, 
          AverageBlue),
        names.arg=c("Green","Red", "Blue"),col=c("green","red", "blue"),main="The Performance")
hist(RedOutcome)
head(RedOutcome)

#Add a 50-50 Green+Red

Returns=matrix(c(0.8, 0.06, 0.9, 0.9, 0.2, 1, 1.05, 1, 1, 1.1, 3, 1, 1.2, 3, 1, 1.4, 3, 1.1), nrow=6, ncol=3, byrow=TRUE)
Pink=Green=Red=Blue=1000

PinkOutcome=GreenOutcome=RedOutcome=BlueOutcome=rep(0, times=1000)

for (j in 1:1000) {
  
  for (i in 1:20) {
    Outcome = sample(1:6,1)
    Green=Returns[Outcome,1]*Green
    Red=Returns[Outcome,2]*Red
    Blue=Returns[Outcome,3]*Blue
    
    ##Every year the money is halved and invested equally in Red and Green
    
    Pink=(Returns[Outcome,1]*Pink/2)+
      (Returns[Outcome,2]*Pink/2)
  }
  
  GreenOutcome[j]=Green
  RedOutcome[j]=Red
  BlueOutcome[j]=Blue
  PinkOutcome[j]=Pink
  
  Pink=Green=Red=Blue=1000
  
}

AverageGreen=median(GreenOutcome)
AverageRed=median(RedOutcome)
AverageBlue=median(BlueOutcome)
AveragePink=median(PinkOutcome)


barplot(c(AverageGreen,AverageRed, AverageBlue, AveragePink),names.arg=c("Green","Red", "Blue", "Pink"),col=c("green","red", "blue", "Pink"),main="The Performance")


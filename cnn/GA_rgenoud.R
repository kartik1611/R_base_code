rm(list=ls(all=TRUE))
par(mfrow=c(1,1))
setwd("C:/Users/admin/Desktop/RegularBatch")

library(rgenoud)

fn=function(x){
  y=4*x^2-8*x
}

minima<- genoud(fn, nvars=1,
                pop.size=100,
                max=FALSE)

fn=function(x){
  y=8*x-4*x^2 
}

maxima<- genoud(fn, nvars=1,
                pop.size=100,
                max=TRUE)

fn=function(x){
  y=3*x^3-9*x
  return(y)
}

minima = genoud(fn, nvars=1, 
                pop.size=100, 
                max=FALSE)

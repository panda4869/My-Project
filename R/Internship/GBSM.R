##
install.packages("sde")
library(MASS)
library(stats4)
library(fda)
library(splines)
library(Matrix)
library(sde)
GBM.matrix<-matrix(nrow=1000,ncol=251)
for(i in 1:1000){
  
  GBM.matrix[i,] <-GBM(x=1, r=0.13, sigma=0.27,T=1,N=250)
  
}

plot(1:251,GBM.matrix[1,],type= "l", xlab = "Time Interval",ylab="Predicted Value",ylim=c(0,3))
for(i in 2:1000){
  lines(1:251,GBM.matrix[i,],col=i)
}
#problem 1
# solve for the equilibrium state
a<-matrix(c(-0.4,0.3,0,0.4,-0.3,0,1,1,1),3,3,byrow=TRUE)
rref(a)

#problem 2#
#a#
x<-seq(0,4,0.1)
density<-vector()
for(i in 1:length(x))
{
  density[i]<-exp(-x[i])
  
}
plot(x,density,type="l",main="exponential distribution")

#b#
plot(x,density,type="l",main="exponential distribution",xaxs="i",yaxs="i",xlim=c(0,4.1))
points(x[11],density[11],col="red")
points(x[21],density[21],col="red")
points(x[41],density[41],col="red")
likelihood<-density[11]*density[21]*density[41]
#c#

x1<-seq(0,4,0.1)
density1<-vector()
for(i in 1:length(x1))
{
  density1[i]<-2*exp(-2*x[i])
  
}
likelihood1<-density1[11]*density1[21]*density1[41]
likelihood1
#g#
#initial the value#
x<-rexp(256,1)
alpha1<-2
beta1<-0.2
#function for plot#
plotgamma<-function(x,alpha1,beta1,n){
  #use gamma to compute the posterior#
  theta<-seq(0,4,0.01)

  #compute the density function#
  den.theta<-dgamma(theta,n+alpha1,sum(x[1:n])+beta1)
  plot(theta,den.theta,ylab="density",xlim=c(0,4),type="l",ylim=c(0,6),xaxs="i",yaxs="i")
  
}


#function for lines#
linegamma<-function(x,alpha1,beta1,n,co){
  theta<-seq(0,4,0.01)
 
  den.theta<-dgamma(theta,n+alpha1,sum(x[1:n])+beta1)
  lines(theta,den.theta,ylab="density",xlim=c(0,4),type="l",col=co)
  
}
#make the plot#
plotgamma(x,2,0.2,4)
linegamma(x,2,0.2,8,"green")
linegamma(x,2,0.2,16,"red")
linegamma(x,2,0.2,256,"blue")
legend("topright",c("n=4","n=8","n=16","n=256"),lty=c(1,1,1,1),col=c("black","green","red","blue"),bty="n",cex=0.9)
title("posterior distribution")



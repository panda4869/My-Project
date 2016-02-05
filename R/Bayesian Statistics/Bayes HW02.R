#3.8#
#simulation#
library(MASS)
#initialize the sample value#
y<-c(16/74,9/99,10/58,13/70,19/122,20/77,18/104,17/129,35/308,55/119)
z<-c(12/125,1/19,2/16,4/48,9/217,7/74,9/38,8/162)
alpha<-seq(1,20,0.5)
beta1<-seq(1,50,0.5)
#initialize the posterior matrix#
posterior2<-posterior1<-matrix(rep(0,length(alpha)*length(beta1)),length(alpha),length(beta1),byrow=T)
#calculate the posterior distribution#
for(i in 1: length(alpha)){
  
  for (j in 1:length(beta1)){
    
    posterior1[i,j]<-prod(dbeta(y,alpha[i],beta1[j]))
    posterior2[i,j]<-prod(dbeta(z,alpha[i],beta1[j]))
  } 
  
}
#normlize the posterior distribution#
posterior1<-posterior1/sum(posterior1)
posterior2<-posterior2/sum(posterior2)
par(mfrow=c(1,2))
postmax1<-max(posterior1)
postmax2<-max(posterior2)
#plot the contour#
result<-contour(alpha,beta1,posterior1,drawlabels=F,levels=seq(0.05,0.95,0.1)*postmax1,xlab="alpha",ylab="beta",xlim=c(0,10),ylim=c(0,40))
title("posterior distribution for y")
result<-contour(alpha,beta1,posterior2,drawlabels=F,levels=seq(0.05,0.95,0.1)*postmax2,xlab="alpha",ylab="beta",xlim=c(0,10))
title("posterior distribution for z")
#simulation for theta y#
post.alpha<-apply(posterior1,1,sum)
alpha1.sim<-vector()
beta1.sim<-vector()
for(s in 1 : 1000){
  #randomly choose the alpha#
  ind1<-sample(x=length(alpha),size=1,prob=post.alpha)
  alpha1.sim[s]<-alpha[ind1]
  #find the beta under the probability p(beta|alpha,y)#
  ind2<-sample(x=length(beta1),size=1,prob=posterior1[ind1,])
  beta1.sim[s]<-beta1[ind2]

}

#plot(alpha1.sim,beta1.sim,xlim=c(0,20),ylim=c(0,50))
#plot the histogram of the alpha & beta for theta y#
par(mfrow=c(2,1))
hist(alpha1.sim,xlab="alpha")
hist(beta1.sim,xlab="beta")

#simulation for theta z#
post.alpha<-apply(posterior2,1,sum)
alpha2.sim<-vector()
beta2.sim<-vector()
for(s in 1 : 1000){
  
  ind1<-sample(x=length(alpha),size=1,prob=post.alpha)
  alpha2.sim[s]<-alpha[ind1]
  ind2<-sample(x=length(beta1),size=1,prob=posterior2[ind1,])
  beta2.sim[s]<-beta1[ind2]
  
}

#plot(alpha2.sim,beta2.sim,xlim=c(0,20),ylim=c(0,50))
#plot the histogram of the alpha & beta for theta y#
par(mfrow=c(2,1))
hist(alpha2.sim,xlab="alpha",breaks=seq(0,10,1))
axis(1,at=seq(0,10,2))
hist(beta2.sim,xlab="beta")

#d#
#calculate the mean of the posterior distribution#
mu.y<-alpha1.sim/(alpha1.sim+beta1.sim)
mu.z<-alpha2.sim/(alpha2.sim+beta2.sim)

delta<-mu.y-mu.z
hist(delta,xlab="mu.y-mu.z")

#Q3.11#
#sample from the multivarite normal distribution#
set.seed(1)
mvn<-mvrnorm(n=1000,mu=c(0,10),Sigma=matrix(c(4,10,10,100),2,2))
plot(mvn,xlab="alpha",ylab="beta")
title("prior distribution")
alpha1<-as.vector(mvn[,1])
beta2<- as.vector(mvn[,2])
#calculate the density function for the prior#
dent<-mat.or.vec(1000,1000)
for(i in 1:1000)
{
  for(j in 1: 1000){
    dent[i,j]<-dmvnorm(x=c(alpha1[i],beta2[j]),mean=c(0,10),sigma=matrix(c(4,10,10,100),2,2))
  }  
  
}
#calculate the posterior distribuion#
x<-c(-0.86,-0.3,-0.05,-0.73)
y<-c(0,1,3,5)
n<-c(5,5,5,5)
posterior3<-matrix(rep(0,length(alpha1)*length(beta2)),length(alpha1),length(beta2),byrow=T)
for(i in 1: length(alpha1)){
  for (j in 1:length(beta2)){
    theta<-inv.logit(alpha1[i]+beta2[j]*x)
    posterior3[i,j]<-prod(theta^y*(1-theta)^(n-y))*dmvnorm(x=c(alpha1[i],beta2[j]),mean=c(0,10),sigma=matrix(c(4,10,10,100),2,2))
  }  
}
#calculate the likelihood#
likelihood<-matrix(rep(0,length(alpha1)*length(beta2)),length(alpha1),length(beta2),byrow=T)
for(i in 1: length(alpha1)){
  for (j in 1:length(beta2)){
    theta<-inv.logit(alpha1[i]+beta2[j]*x)
    likelihood[i,j]<-prod(theta^y*(1-theta)^(n-y))
  }  
}

#re-order in an monotonic order the data otherwise we couldn't use contour function#
a<-sort(alpha1)
b<-sort(beta2)
a.ad<-vector()
b.bd<-vector()
likelihood1<-dent1<-posterior4<-matrix(rep(0,length(a)*length(b)),length(a),length(b),byrow=T)
for(i in 1:1000){
for(j in 1:1000){
  a.ad<-which(alpha1==a[i])
  b.bd<-which(beta2==b[j])
  posterior4[i,j]<-posterior3[a.ad,b.bd]
  dent1[i,j]<-dent[a.ad,b.bd]
  likelihood1[i,j]<-likelihood[a.ad,b.bd]
}
}
par(mfrow=c(1,2))
contour(x=a,y=b,z=dent1,drawlabels=FALSE,xlab="alpha",ylab="beta",xlim=c(-6,6),ylim=c(-5,10))
title("prior distribution")
contour(x=a,y=b,z=likelihood1,drawlabels=FALSE,xlab="alpha",ylab="beta",xlim=c(-2,2),ylim=c(-5,5))
title("likelihood")
#draw 1000 numbers from the posterior distribution#

post.alpha<-apply(posterior3,1,sum)
alpha3.sim<-vector()
beta3.sim<-vector()
for(s in 1 : 1000){
  
  ind1<-sample(x=length(alpha1),size=1,prob=post.alpha)
  alpha3.sim[s]<-alpha1[ind1]
  ind2<-sample(x=length(beta2),size=1,prob=posterior3[ind1,])
  beta3.sim[s]<-beta2[ind2]
  
}
postmax3<-max(posterior4)
par(mfrow=c(1,2))
result<-contour(x=a,y=b,z=posterior4,drawlabels=FALSE,levels=seq(0.05,0.95,0.1)*postmax3,xlab="alpha",ylab="beta",xlim=c(-2,2),ylim=c(-4,4))
title("posterior distribution")
plot(alpha3.sim,beta3.sim,xlab="alpha.sim",ylab="beta.sim",xlim=c(-2,2),ylim=c(-4,4))
title("1000 simulations")
#report the p(beta>0|y)#
sum(beta3.sim>0)/(length(beta3.sim))
beta3<-beta3.sim[beta3.sim>0]
LD50<-matrix(rep(0,1000*length(beta3)),1000,length(beta3),byrow=T)

for(i in 1:1000){
  for (j in 1:length(beta3)){
    LD50[i,j]<--alpha3.sim[i]/beta3[j] 
  }

}
  hist(log(LD50))


##
sum(beta2>0)/(length(beta2))
beta4<-beta2[beta2>0]
LD50.piror<-matrix(rep(0,1000*length(beta4)),1000,length(beta4),byrow=T)

for(i in 1:1000){
  for (j in 1:length(beta4)){
    LD50.piror[i,j]<--alpha1[i]/beta4[j] 
  }
  
}

par(mfrow=c(1,2))
hist(log(LD50.piror))
hist(log(LD50))


#4.1#
y=c(-2,-1,0,1.5,2.5)
#use newton's method to solve the nonlinear equation#
newton.method<-function(theta,y){
  Ep<-10
  while(abs(Ep)>0.001){
    theta1<-theta-(sum(2*(y-theta)/(1+(y-theta)^2)))/(sum((-2+2*(y-theta)^2)/(1+(y-theta)^2)^2))
    Ep<-theta1-theta
    theta<-theta1
    
  }
return(theta1)  
}
theta<-newton.method(1,y)
s<-seq(-10,10,0.1)
id=-(sum((-2+2*(y-theta)^2)/(1+(y-theta)^2)^2))

#simulate the posterior#
posterior.cauchy<-vector()
for(i in 1:length(s)){
  
  posterior.cauchy[i]=prod(dcauchy(y,(location=s[i])))

}
posterior.cauchy<-posterior.cauchy/sum(posterior.cauchy)
#plot the approximate normal density vs the real cauchy density function#
plot(s,dnorm(s,mean=theta,sd=(1/id)^0.5)/sum(dnorm(s,mean=theta,sd=(1/id)^0.5)),type="l",xlim=c(-5,5),ylab="denstity")
lines(s,posterior.cauchy,col="blue")
legend("topleft",c("Approximated Normal","Cauchy Distribution"),lty=c(1,1),col=c("black","blue"),bty="n")
title("Approximated Normal vs Cauchy Distribution")



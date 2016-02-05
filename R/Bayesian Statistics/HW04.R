#5.13b#
#simulation#
y<-c(16,9,10,13,19,20,18,17,35,55)
n<-c(74,99,58,70,122,77,104,129,308,119)
logratio<-seq(-2.5,-0.5,0.01)
logsum<-seq(1,4,0.01)
posterior1<-mat.or.vec(length(logratio),length(logsum))
for(i in 1: length(logratio)){
 
  for (j in 1:length(logsum)){
    b<-exp(logsum[j])/(1+exp(logratio[i]))
    a<-b*exp(logratio[i])
    posterior1[i,j]<-exp(log(a*b)-2.5*log(a+b)+sum(lgamma(a+b)+lgamma(a+y)+lgamma(b+n-y)-lgamma(a)-lgamma(b)-lgamma(a+b+n)))

  } 
  
}


post.logratio<-apply(posterior1,1,sum)
logratio.sim<-vector()
logsum.sim<-vector()
for(s in 1 : 1000){
  #randomly choose the logratio#
  ind1<-sample(x=length(logratio),size=1,prob=post.logratio)
  logratio.sim[s]<-logratio[ind1]
  #find the beta under the probability p(logsum|logratio,y)#
  ind2<-sample(x=length(logsum),size=1,prob=posterior1[ind1,])
  logsum.sim[s]<-logsum[ind2]
  
} 
par(mfrow=c(1,2))
result<-contour(logratio,logsum,posterior1,drawlabels=F,xlab="logratio",ylab="logsum",ylim=c(1,4),main="posterior distribution for logratio and logsum")
plot(logratio.sim,logsum.sim,xlab="logratio",ylab="logsum",main="simulation result",ylim=c(1,4))

alpha1.sim<-vector()
beta1.sim<-vector()
for(i in 1: 1000){
  
    beta1.sim[i]<-exp(logsum.sim[i])/(1+exp(logratio.sim[i]))
    alpha1.sim[i]<-beta1.sim[i]*exp(logratio.sim[i])
    
  } 
  


plot(alpha1.sim,beta1.sim,xlab="alpha",ylab="beta",main="simulation result for alpha beta")
#c#
theta<-mat.or.vec(1000,10)
for(i in 1: 1000){
  for(j in 1:10){
  set.seed(i)
  theta[i,j]<-rbeta(1,alpha1.sim[i]+y[j],beta1.sim[i]+n[j]-y[j])
  
  }
} 
raw.pro<-y/n
m<-apply(theta,2,median)

y1<-matrix(rep(raw.pro,1),1000,10,byrow=T)
plot(y1,theta,xlab="y/n",ylab="posterior theata",xaxs="i",yaxs="i",ylim=c(0,0.5),xlim=c(0,0.5))
lines(c(0,0.5),c(0,0.5))
#d#
quantile(theta,c(0.025,0.975))

#14b#
n<-c(74,99,58,70,122,77,104,129,308,119)
logratio1<-seq(0,9,0.01)
logsum1<-seq(-1,3,0.01)
posterior2<-mat.or.vec(length(logratio1),length(logsum1))
for(i in 1: length(logratio1)){
  
  for (j in 1:length(logsum1)){
    b<-exp(logsum1[j])/(1+exp(logratio1[i]))
    a<-b*exp(logratio1[i])
    posterior2[i,j]<--2.5*log(a+b)+log(a*b)+sum(log(b^a)+lgamma(a+n)-lgamma(a)-log((b+1)^(a+n)))
    
  } 
  
}
posterior2<-exp(posterior2-max(posterior2))
post.logratio1<-apply(posterior2,1,sum)
logratio.sim1<-vector()
logsum.sim1<-vector()
for(s in 1 : 1000){
  #randomly choose the logratio#
  ind1<-sample(x=length(logratio1),size=1,prob=post.logratio1)
  logratio.sim1[s]<-logratio1[ind1]
  #find the beta under the probability p(logsum|logratio,y)#
  ind2<-sample(x=length(logsum1),size=1,prob=posterior2[ind1,])
  logsum.sim1[s]<-logsum1[ind2]
  
} 
par(mfrow=c(1,2))
result<-contour(logratio1,logsum1,posterior2,drawlabels=F,xlab="logratio",ylab="logsum",main="posterior distribution for logratio and logsum",xlim=c(4,5.5),ylim=c(0,2.5))
plot(logratio.sim1,logsum.sim1,xlab="logratio",ylab="logsum",main="simulation result for logratio and logsum",ylim=c(0,2.5),xlim=c(4,5.5))
#transform to the alpha beta form#
alpha1.sim1<-vector()
beta1.sim1<-vector()
for(i in 1: 1000){
  
  beta1.sim1[i]<-exp(logsum.sim1[i])/(1+exp(logratio.sim1[i]))
  alpha1.sim1[i]<-beta1.sim1[i]*exp(logratio.sim1[i])
  
} 

plot(alpha1.sim1,beta1.sim1,xlab="alpha",ylab="beta",main="simulation result for alpha beta")
#e#
theta1<-mat.or.vec(1000,10)
for(i in 1: 1000){
  for(j in 1:10){
    set.seed(i)
    theta1[i,j]<-rgamma(1,alpha1.sim1[i]+n[j],beta1.sim1[i]+1)
    
  }
} 
y2<-matrix(rep(n,1),1000,10,byrow=T)
par(mfrow=c(1,2))
plot(y2,theta1,xlab="n",ylab="posterior theta",xaxs="i",yaxs="i",ylim=c(0,300),xlim=c(0,310),main="posterior theta vs n")
lines(c(0,400),c(0,400))
hist(theta1,main="histogram for posterior theta")
quantile(theta1,c(0.025,0.975))

#5.15#
data<-read.table("meta.asc")
y3<-log(data$V4)-log(data$V5-data$V4)-log(data$V2)+log(data$V3-data$V2)
sigma<-1/data$V4+1/(data$V5-data$V4)+1/data$V2+1/(data$V3-data$V2)
tau<-seq(0.001,0.5,0.001)
posterior3<-vector()

for(i in 1: length(tau)){
mu.hat<-sum(1/(sigma+tau[i]^2)*y3)/sum(1/(sigma+tau[i]^2))
V.mu<-sum(1/(sigma+tau[i]^2))^(-0.5)
posterior3[i]<-V.mu*prod((sigma+tau[i]^2)^(-0.5)*exp(-0.5*(y3-mu.hat)^2/(sigma+tau[i]^2)))  
}
posterior3<-posterior3/sum(posterior3)
plot(tau,posterior3,xaxs="i",yaxt="n",ylab="",xlab="tau",main="marginal posterior for tau")
#b#
mu.con<-vector()
theta.j<-mat.or.vec(length(tau),length(y3))
theta.sd<-mat.or.vec(length(tau),length(y3))
for(i in 1: length(tau)){
V.mu<-(1/sum(1/(sigma+tau[i]^2)))
mu.con<-sum(y3/(sigma+tau[i]^2))/sum(1/(sigma+tau[i]^2))
for(j in 1:length(y3)){
theta.j[i,j]<-(y3[j]/sigma[j]+mu.con/tau[i]^2)/(1/sigma[j]+1/tau[i]^2)
theta.sd[i,j]<-(1/(1/sigma[j]+1/tau[i]^2)+((1/tau[i]^2)/(1/sigma[j]+1/tau[i]^2))^2*V.mu)^0.5
}
}
tau.matrix<-matrix(rep(tau),length(tau),length(y3),byrow=FALSE)
par(mfrow=c(1,2))
matplot(tau.matrix,theta.j,type="l",xlab="tau",ylab="estimated effect")
matplot(tau.matrix,theta.sd,type="l",xlab="tau",ylab="standard deviation of estimated effect")

#c#
#no pooling#
theta.med<-mat.or.vec(length(tau),length(y3))
for(i in 1: length(tau)){
  
  
  mu.con<-sum(1/(sigma+tau[i]^2)*y3)/sum(1/(sigma+tau[i]^2))
  for(j in 1:length(y3)){
    theta.med[i,j]<-median((y3[j]/sigma[j]+mu.con/tau[i])/(1/sigma[j]+1/tau[i]))
   
  }
}
theta.med1<-apply(theta.med,2,median)
plot(y3,theta.med1,xlab="crude effect estimate", ylab="posterior median effect estimates",main="crude effect estimate vs posterior median effect estimates",ylim=c(-0.5,0.5),xlim=c(-0.6,0.3),text(y3,theta.med1,labels=seq(1,22,1),cex=0.8),cex=0.1)
lines(c(-0.8,0.5),c(-0.8,0.5))

#complete pooling#
theta.med<-mat.or.vec(length(tau),length(y3))
for(i in 1: length(tau)){
  
  
  mu.con<-sum(1/(sigma+tau[i]^2)*y3)/sum(1/(sigma+tau[i]^2))
  for(j in 1:length(y3)){
    theta.med[i,j]<-median(rnorm(5000,(y3[j]/sigma[j]+mu.con/tau[i])/(1/sigma[j]+1/tau[i]),(1/(1/sigma[j]+1/tau[i]^2))^0.5))
    
  }
}
theta.med1<-apply(theta.med,2,median)
theta.med2<-median(apply(theta.med,2,median))
theta.med2<-rep(theta.med2,22)
par(mfrow=c(1,2))
plot(theta.med1,y3,ylab="crude effect estimate", xlab="posterior median effect estimates",main="no pooling",ylim=c(-0.5,0.5),xlim=c(-0.6,0.3),text(theta.med1,y3,labels=seq(1,22,1),cex=0.8),cex=0.1)
lines(c(-0.8,0.5),c(-0.8,0.5))
plot(theta.med2,y3,ylab="crude effect estimate", xlab="posterior median effect estimates",main="complete pooling",ylim=c(-0.5,0.5),xlim=c(-0.6,0.3),text(theta.med2,y3,labels=seq(1,22,1),cex=0.8),cex=0.1)
lines(c(-0.8,0.5),c(-0.8,0.5))
#d#
#simulation#

tau.sim1<-vector()
mu.sim1<-vector()
theta.j<-vector()
for(s in 1 : 1000){  
  ind1<-sample(x=length(tau),size=1,prob=posterior3)
  tau.sim1[s]<-tau[ind1]
  mu.hat<-sum(1/(sigma+tau.sim1[s]^2)*y3)/sum(1/(sigma+tau.sim1[s]^2))
  V.mu<-(1/sum(1/(sigma+tau.sim1[s]^2)))^0.5
  mu.sim1[s]<-rnorm(1,mu.hat,V.mu)
  theta.j[s]<-rnorm(1,mu.sim1[s],tau.sim1)
} 

hist(theta.j,main="simulation for new theta",xlab="new theta")


#e#

p1.sim<-vector()
p0.sim<-vector()
y.new<-vector()
for(i in 1: length(theta.j)){
  set.seed(i)
  ind<-sample(1:22,1)
  p1.sim[i]<-exp(theta.j[i])*data$V2[ind]/data$V3[ind]
  p0.sim[i]<-exp(theta.j[i])*data$V4[ind]/data$V5[ind]
  y.new[i]<-rnorm(1,theta.j[i],1/p1.sim[i]+1/(1-p1.sim[i])+1/p0.sim[i]+1/(1-p0.sim[i]))
}
hist(y.new,xlab="new y",main="Outcomes")

hist(theta.j23,xlab="experiment22",main="")
hist(p.j23,xlab="experiment22",main="")



#6.5#

switches<-vector()
for(i in 1:1000){
set.seed(i)
 theta<-rbeta(1,8,14)
 yrep<-vector()
 while(sum(yrep==0)<13)
  yrep<-c(yrep,rbinom(1,1,theta))
 count<-vector()
 for(j in 1 : length(yrep)-1)
 { 
   count[j]<-ifelse(yrep[j]!=yrep[j+1],1,0)
     
 }
 switches[i]<-sum(count)
 
}


tests<-vector()
for(i in 1:1000){
set.seed(i)
 theta<-rbeta(1,8,14)
 yrep<-vector()
 while(sum(yrep==0)<13 & length(yrep)<20)
  yrep<-c(yrep,rbinom(1,1,theta))
 count<-vector()
 for(j in 1 : length(yrep)-1)
 { 
   count[j]<-ifelse(yrep[j]!=yrep[j+1],1,0)
     
 }
 tests[i]<-sum(count)
 
}
par(mfrow=c(1,2))
hist(switches,main="distribution of number of switches")
hist(tests,main="fig 6.5")

#6.7#
test.stats<-vector()
y.hat<-vector()
for (i in 1:1000){
 theta.post<-rnorm(1,5.1,1/10)
  y.hat<-rnorm(100,theta.post,1)
 test.stats[i]<-max(abs(y.hat))

}
hist(test.stats,main="",xlab="T(yrep)")
length(which(test.stats>8.1))/length(test.stats)

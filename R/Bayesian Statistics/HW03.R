set.seed(1)
rgamma(1,238,10)
set.seed(1)
rgamma(1,238,1/10)
set.seed(1)
rgamma(1,238)/10
#do the regression first#
t<-seq(0,9,1)
y<-c(24,25,31,31,22,21,26,20,16,22)
d<-data.frame(t=t,y=y)
lm1<-lm(y~t+1,data=d)
summary(lm1)
coef1<-summary(lm1)$coef[,1]
err<-summary(lm1)$coef[,2]
#(b)#
#draw numbers from the bivariate normal distribution based on the linear regression result#
set.seed(1)
mvn<-mvrnorm(n=1000,mu=c(coef1[1],coef1[2]),Sigma=matrix(c(err[1],0,0,err[2]),2,2))
alpha1<-as.vector(mvn[,1])
beta2<- as.vector(mvn[,2])
density<-mat.or.vec(length(alpha1),length(beta2))
for(i in 1:length(alpha1)){
  
  for(j in 1:length(beta2)){
    density[i,j]<-dmvnorm(x=c(alpha1[i],beta2[j]),mean=c(coef1[1],coef1[2]),sigma=matrix(c(err[1],0,0,err[2]),2,2))
    
  }
}
#re-order in an monotonic order the data otherwise we couldn't use contour function#
a<-sort(alpha1)
b<-sort(beta2)
a.ad<-vector()
b.bd<-vector()
dent1<-matrix(rep(0,length(a)*length(b)),length(a),length(b),byrow=T)
for(i in 1:1000){
  for(j in 1:1000){
    a.ad<-which(alpha1==a[i])
    b.bd<-which(beta2==b[j])
    dent1[i,j]<-density[a.ad,b.bd]
    
  }
}
#plot the contour#
contour(x=a,y=b,z=dent1,xlab="alpha",ylab="beta",drawlabel=FALSE)
title("Plot of the bivairate normal prior ")


#(d)#
#input the data#
#do the simple linear regression first#
t<-seq(0,9,1)
y<-c(24,25,31,31,22,21,26,20,16,22)
d<-data.frame(t=t,y=y)
lm1<-lm(y~t+1,data=d)
coef1<-summary(lm1)$coef[,1]
err<-summary(lm1)$coef[,2]

#generate the prior#
#we choose the range of the parameters based on our regression result#
alpha2<-seq(coef1[1]-4*err[1],coef1[1]+4*err[1],length=100)
beta1<-seq(coef1[2]-4*err[2],coef1[2]+4*err[2],length=100)
posterior<-mat.or.vec(length(alpha2),length(beta1))
for(i in 1: length(alpha2)){
  for (j in 1:length(beta1)){
    theta<-alpha2[i]+beta1[j]*t
    posterior[i,j]<-prod(theta^y*exp(-theta))
  }  
}
posterior1<-posterior/sum(posterior)
contour(x=alpha2,y=beta1,z=posterior1,drawlabels=FALSE,xlab="alpha",ylab="beta")
title("contour plot for the posterior distribution")
#the range of theta#
range(alpha2+t*beta1)
#e#
d<-data.frame(t=t,y=y)
lm1<-lm(y~t+1,data=d)
summary(lm1)
#(f)#
#draw points from the posterior distribution#
post.alpha<-apply(posterior1,1,sum)
alpha.sim<-vector()
beta.sim<-vector()
for(s in 1 : 1000){
  
  ind1<-sample(x=length(alpha2),size=1,prob=post.alpha)
  alpha.sim[s]<-alpha2[ind1]
  ind2<-sample(x=length(beta1),size=1,prob=posterior1[ind1,])
  beta.sim[s]<-beta1[ind2]
}

contour(x=alpha2,y=beta1,z=posterior1,drawlabels=FALSE,xlab="alpha",ylab="beta")
points(x=alpha.sim,y=beta.sim,cex=0.5)
title("simulation from the posterior")
#(g)#
h<-alpha.sim+10*beta.sim
hist(h,xlab="expected number of accedents")
#(h)#
#predict the number of fatal accidents using poisson distribution#
num<-rpois(1000,h)
quantile(num,c(0.025,0.975))
#i#
range(alpha.sim)
range(a)
range(beta.sim)
range(b)
par(mfrow=c(1,2))
contour(x=a,y=b,z=dent1,xlab="alpha",ylab="beta",drawlabel=FALSE)
title("Plot of the bivairate normal prior ")
contour(x=alpha2,y=beta1,z=posterior1,drawlabels=FALSE,xlab="alpha",ylab="beta")
title("contour plot for the posterior distribution")
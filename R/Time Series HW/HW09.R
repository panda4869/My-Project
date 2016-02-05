#Q1(1)#
pnorm(q=log(990/1000),mean=0.001,sd=0.015)
#Q1(2)#
pnorm(q=log(990/1000),mean=0.005,sd=5^0.5*0.015)
dens <- function (y, th){
  dens0 <- NULL
  for (i in 1:length(th))
    dens0 <- c(dens0, prod (dcauchy (y, th[i], 1)))
  dens0}
y <- c(-2, -1, 0, 1.5, 2.5)
step <- .01
theta <- seq(step/2, 1-step/2, step)
dens.unnorm <- dens(y,theta)
dens.norm <- dens.unnorm/(step*sum(dens.unnorm))
plot (theta, dens.norm, ylim=c(0,1.1*max(dens.norm)), type="l", xlab="theta", ylab="normalized density",xaxs="i", yaxs="i", cex=2)

thetas <- sample (theta, 1000, step*dens.norm,replace=TRUE)
hist (thetas, xlab="theta", yaxt="n",breaks=seq(0,1,.05), cex=2)


bondvalue = function(c,T,r,par)
{
  #       Computes bv = bond values (current prices) corresponding
  #       to all values of yield to maturity in the
  #       input vector r
  #
  # INPUT
  #        c = coupon payment (semiannual)
  #        T = time to maturity (in years)
  #        r = vector of yields to maturity (semiannual rates)
  #        par = par value
  #
  bv = c/r + (par - c/r) * (1+r)^(-2*T)
  bv
}
price = 1020 #
C=26 #
T=4 #
par = 1000 #
r = seq(.02,.05,length=300)
value = bondvalue(C,T,r,par)
yield2M = spline(value,r,xout=price) # spline interpolation

yield2M=uniroot(function(r) r^2-.5, c(0.7,0.8))

plot(r,value,xlab='yield to maturity',ylab='price of bond',type="l",main="par = 1000, coupon payment = 26, T = 4",lwd=2)
abline(h=1020)
abline(v=yield2M)
yield2M=uniroot(function(r) 280/r+(10000-280/r)*(1+r)^(-16)-9800, c(0.010,0.050))

yield2M=uniroot(function(r) 40/r+(1000-40/r)*(1+r)^(-30)-1200, c(0.010,0.050))

par(mfrow=c(2,1))
plot(newdata$proportion,xaxt="n",xlab='State', ylab='liberal_proportion',col='red')
axis(1,line=0,at=seq(1,48,1),label=AB,cex.axis=0.47)
title("liberal_proportion in each state")
plot(newdata$vote_Obama_pct,xaxt="n",xlab='State', ylab='vote_Obama_pct ',col='blue')
axis(1,line=0,at=seq(1,48,1),label=AB,cex.axis=0.47)
title("vote_Obama_pct in each state")


#CH11#
install.packages("Ecdat")
install.packages("quadprog")
library(Ecfun)
library(Ecdat)
library(quadprog)
#CH11#
#read data#
dat = read.csv("Stock_FX_Bond.csv",header=T)
prices = cbind(dat$GM_AC,dat$F_AC,dat$CAT_AC,dat$UTX_AC,dat$MRK_AC,dat$IBM_AC)
n = dim(prices)[1]
returns =  100*(prices[2:n,]/prices[1:(n-1),]-1)
pairs(returns)
mean_vect = apply(returns,2,mean)
cov_mat = cov(returns)
sd_vect = sqrt(diag(cov_mat))

##

weights = matrix(0,nrow=1,ncol=6) # storage for portfolio weights
Amat = cbind(rep(1,6),-rep(1,6),rep(1,6)) # set the constraint matrix Aeq
bvec = c(1,-3,-0.6)  # constraint vector beq
result =solve.QP(Dmat=2*cov_mat,dvec=rep(0,6),Amat=Amat,bvec=bvec,meq=1)
sdP = sqrt(result$value)
weights[1,] = result$solution


pdf("quad_prog_plot.pdf",width=6,height=5)  #  Figure 11.3
plot(sdP,muP,type="l",xlim=c(0,2.5),ylim=c(0,.15),lty=3)  #  plot
# the efficient frontier (and inefficient frontier)
mufree = 3/365 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=4,lty=2)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier
text(sd_vect[1],mean_vect[1],"GE",cex=1.5)
text(sd_vect[2],mean_vect[2],"IBM",cex=1.5)
text(sd_vect[3],mean_vect[3],"Mobil",cex=1.5)

muc=1.031+rt(1000,31)*0.24/((32)^0.5)
mut=1.173+rt(1000,36)*0.20/((36)^0.5)
hist(mut-muc,xlab="mut-muc")
sort(mut-muc)[c(25,975)]
dev.off()


cost<-2^seq(-10,0,0.5)

folds<-dim(data1)[1]/5
max.train_size<-dim(data1)[1]
error.rate<-matrix(rep(0,length(cost)*1),length(cost),1)
for(j in 1:length(cost)){
  error<-0
  for(i in 1:5){
    
    ind.test<-c(1: max.train_size)[(folds*(i-1)+1):(folds*i)]
   
    ind.train<-c(1: max.train_size)[-ind.test]
    
    svm.train<-svm(data1[ind.train,],label[ind.train],type='C',kernel='linear',cost=cost[j],gamma=0)
    svm.pred<-predict(svm.train,data1[ind.test,])
    error=error+sum(svm.pred!=label[ind.test])
    
  }
  error
  error.rate[j,]=error/max.train_size
}

cost<-2^seq(-10,0,0.5)
g=c(0.0001,0.001,0.01,0.1)
folds<-dim(data1)[1]/5
max.train_size<-dim(data1)[1]
error.rate<-matrix(rep(0,length(cost)*length(g)),length(g),length(cost))
for(l in 1:length(g)){
  
  for(j in 1:length(cost)){
    error<-0
    for(i in 1:5){
      
      ind.test<-c(1: max.train_size)[(folds*(i-1)+1):(folds*i)]
      ind.train<-c(1: max.train_size)[-ind.test]
      svm.train<-svm(data1[ind.train,],label[ind.train,],type='C',kernel='radial',cost=cost[j],gamma=g[l])
      svm.pred<-predict(svm.train, data1[ind.test,])
      error=error+sum(svm.pred!= label[ind.test,])
    }
    error.rate[l,j]=error/max.train_size
  }  
}





##

library(rJava)
library(xlsxjars)
library(xlsx)
data<-read.xlsx(file="CAPM-DATA.xlsx",sheetName="capm")
data1<-cbind(data[,1:5]-data[,6])
colnames(data1)=c("MSOFT1","GE1","GM1","IBM1","MPORT1")
lm1<-lm(MSOFT1~1+MPORT1,data=data1)
summary(lm1)


beta1<-vector()
for(i in 1:4){
     lm1<-lm(data1[,i]~data1[,5]+1,data=data1)
    beta1[i]<-lm1$coefficients[2]
   
     }

betas<-data.frame(beta=beta1)
rownames(betas)<-colnames(data1)[1:4]



lm1<-lm(MSOFT1~MPORT1+1,data=data1)
summary(lm1)
confint(lm1,'MPORT1',level=0.95)
lm2<-lm(GE1~MPORT1+1,data=data1)
summary(lm2)
confint(lm2,'MPORT1',level=0.95)
lm3<-lm(GM1~MPORT1+1,data=data1)
summary(lm3)
confint(lm3,'MPORT1',level=0.95)
lm4<-lm(IBM1~MPORT1+1,data=data1)
summary(lm4)
confint(lm4,'MPORT1',level=0.95)



confint(lm1,'MPORT1',level=0.95)
confint(lm2,'MPORT1',level=0.95)
confint(lm3,'MPORT1',level=0.95)
confint(lm4,'MPORT1',level=0.95)



library("car")
linearHypothesis(lm1,c("MPORT1"),c(1))
linearHypothesis(lm2,c("MPORT1"),c(1))
linearHypothesis(lm3,c("MPORT1"),c(1))
linearHypothesis(lm4,c("MPORT1"),c(1))



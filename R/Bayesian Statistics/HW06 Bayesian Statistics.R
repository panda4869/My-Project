##

  ux <- unique(county)
  mat1 <- matrix (county, nrow=length(county), ncol=length(ux))
  mat2 <- matrix (ux, nrow=length(county), ncol=length(ux), byrow=TRUE)
  (mat1==mat2)*1

y.1 <- c (5.0, 13.0, 7.2, 6.8, 12.8, 5.8, 9.5, 6.0, 3.8, 14.3, 1.8, 6.9, 4.7, 9.5)
y.2 <- c (0.9, 12.9, 2.6, 3.5, 26.6, 1.5, 13.0, 8.8, 19.5, 2.5, 9.0, 13.1, 3.6, 6.9)
y.3 <- c (14.3, 6.9, 7.6, 9.8, 2.6, 43.5, 4.9, 3.5, 4.8, 5.6, 3.5, 3.9, 6.7)
label.1 <- c(1,1,1,1,1,0,1,1,1,0,1,1,1,1)
label.2 <- c(0,1,1,0,1,1,1,1,1,0,1,1,1,0)
label.3 <- c(1,0,1,0,1,1,1,1,1,1,1,1,1)
county <- rep(1:3,c(length(y.1),length(y.2),length(y.3)))
y <- c(y.1,y.2,y.3)
x <- cbind (c(label.1,label.2,label.3), (mat1==mat2)*1)
ls.out <- lsfit (x, log(y), intercept=F)
lsd <- ls.diag (ls.out)
install.packages("asbio")
library(tcltk)
library(asbio)
install.packages("mvtnorm")
library(mvtnorm)
nsim <- 10000
n <- nrow(x)
k <- ncol(x)
sigma <- rep (NA, nsim)
beta <- array (NA, c(nsim, k))
for (i in 1:nsim){
  sigma[i] <- rinvchisq(1,df=n-k,scale=lsd$std.dev^2)# inverse chi squre# 
  beta[i,] <- mvrnorm(1,mu=ls.out$coef, Sigma=sigma[i]*ginv(lsd$cov.unscaled*n))}

output <- exp (cbind (beta[,2], beta[,1]+beta[,2], beta[,3],beta[,1] + beta[,3], beta[,4], beta[,1] + beta[,4], beta[,1], sigma))
for (i in 1:ncol(output)) print (round(quantile(output[,i],c(.25,.5,.75)),1))



for (i in 1:nsim){
  sigma[i] <- sqrt(rinvchisq(1,df=n-k,scale=lsd$std.dev^2))
  beta[i,] <- ls.out$coef + sigma[i]*t(chol(lsd$cov.scaled))%*%rnorm(k)
}
output <- exp (cbind (beta[,2], beta[,1]+beta[,2], beta[,3],
                      beta[,1] + beta[,3], beta[,4], beta[,1] + beta[,4], beta[,1], sigma))
for (i in 1:ncol(output)) print (round(quantile(output[,i],c(.25,.5,.75)),1))
  
#compute the posterior #
  theta <- rbeta (10000, 13, 3)
#simulate the prob of basement#
  b <- rbinom (10000, 1, theta)
  #draw number for y#
  logy.new <- rnorm (10000, beta[,3] + b*beta[,1], sigma)
  y.new <- exp(logy.new)
 #show the quantile#
  quantile(y.new,0.025)
  quantile(y.new,0.975)
  hist (y.new[y.new<50], yaxt="n",breaks=0:50,
        xlab="radon lever prediction", main="y.new")
  
  print (round(quantile(y.rep,c(.025,.25,.5,.75,.975)),1))
  hist (y.rep[y.rep<40], yaxt="n", breaks=0:40,
        xlab="radon measurement (new house)", cex=2)
# Code for 2.10（a）
library(MASS)
Boston
?Boston
attach(Boston)

# Code for 2.10（b）
par(mfrow=c(3,3))
plot(zn,crim)
plot(black,crim)
plot(lstat,crim)
plot(rad,indus)
plot(zn,rm)
plot(ptratio,lstat)
plot(lstat,dis)
plot(age,tax)
plot(medv,tax)

# Code for 2.10（c）
>cor(Boston)

# Code for 2.10（d）
cor(Boston)
par(mfrow=c(1,3))
plot(crim)
plot(tax)
plot(ptratio)
# Code for 2.10（e）
table(chas)

# Code for 2.10（f）

which(medv==min(medv),arr.ind = TRUE)

# Code for 2.10（g）
Boston[c(399,406),]

# Code for 2.10（h）
length(which(rm>7,arr.ind = TRUE))
length(which(rm>8,arr.ind = TRUE))
Boston[c(which(rm>8,arr.ind = TRUE)),]
summary(Boston[c(which(rm>8,arr.ind = TRUE)),])
detach(Boston)
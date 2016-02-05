#read data
library(MASS)
data(Boston)
View(Boston)
#use crim zn indus nox rm age tax against medv
#scatter plot
plot(Boston[c('crim', 'zn', 'indus', 'nox', 'rm', 'age', 'tax','medv')])
fit1<-lm(medv~1+crim+zn+indus+nox+rm+age+tax,data=Boston)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)

#functional form
r1$adj.r.squared
r1$r.squared
fit1.1<-lm(log(medv)~1+crim+zn+indus+nox+rm+age+tax,data=Boston)
summary(fit1.1)
plot(fit1.1)
cor(Boston[c('crim', 'zn', 'indus', 'nox', 'rm', 'age', 'tax','medv')])
fit1.2<-lm(medv~1+crim+indus+rm+tax+rm*indus,data=Boston)
summary(fit1.2)
#checking constant variance
par(mfrow=c(1,1))
plot(fitted(fit1),residuals(fit1),xlab="Fitted", ylab="Residuals")
abline(h=0,col='red')
plot(fitted(fit1), abs(residuals(fit1)), xlab="Fitted", ylab="|Residuals|")
abline(h=0,col='red')
par(mfrow=c(2,2))
plot(Boston$crim,residuals(fit1),xlab="Crim", ylab="Residuals")
plot(Boston$rm,residuals(fit1),xlab="rm", ylab="Residuals")
plot(Boston$age,residuals(fit1),xlab="age", ylab="Residuals")
plot(Boston$tax,residuals(fit1),xlab="tax", ylab="Residuals")
#test variance homogeneity
bptest(fit2)
fit2<-lm(log(medv)~1+crim+rm+age+tax,data=Boston)
summary(fit2)
fit2<-glm(medv~1+crim+rm+age+tax,data=Boston,weights=(1/Boston$crim))
summary(fit2)
par(mfrow=c(2,2))
plot(Boston$crim,residuals(fit2),xlab="Crim", ylab="Residuals")
plot(Boston$rm,residuals(fit2),xlab="rm", ylab="Residuals")
plot(Boston$age,residuals(fit2),xlab="age", ylab="Residuals")
plot(Boston$tax,residuals(fit2),xlab="tax", ylab="Residuals")

#regress residual on fitted value
summary(lm(abs(residuals(fit1))~fitted(fit1)))
#check for normality
par(mfrow=c(1,2))
qqnorm(residuals(fit1),ylab="Residuals")
qqline(residuals(fit1)) 
hist(residuals(fit1))
shapiro.test(residuals(fit1))
#outliers and laverage points
library(car)
outlierTest(fit1)
fit4<-lm(log(medv)~1+crim+rm+age+tax,data=Boston,subset=(residuals(fit1)<max(residuals(fit1))))
summary(fit4)
plot(fit4)
#detect high leverage points
fit1_if<-influence(fit1)
dim(Boston)[1]
unique(which(fit1_if$hat>2*(7+1)/dim(Boston)[1]))
#cook's distance
plot(fit1, which=4)
plot(fit1, which=6)
cook<-cooks.distance(fit2)
which.max(cook)
fit5<-lm(log(medv)~1+crim+rm+age+tax,data=Boston,subset=(cook<max(cook)))
summary(fit5)
length(which(cook>4/(506-7-1)))
fit6<-lm(log(medv)~1+crim+rm+age+tax,data=Boston,subset=(cook<4/(506-7-1)))
summary(fit6)
plot(fit6,which=2)
qqnorm(residuals(fit6),ylab="Residuals")
qqline(residuals(fit6)) 
plot(fit1,which=2)
#corrective measure

#check for correlated error
install.packages("orcutt")
library(lmtest)
library(orcutt)
dwtest(log(medv)~1+crim+rm+age+tax,data=Boston)
fit3<-cochrane.orcutt(fit2)
fit3
dwtest(fit3$Cochrane.Orcutt)
# robust linear regression
fit7<-lqs(medv~1+crim+zn+indus+nox+rm+age+tax,data=Boston,method=c('lms'))
fit7<-lmsreg(medv~1+crim+zn+indus+nox+rm+age+tax,data=Boston)
fit7
fit7$call
plot(fit7$model)
names(fit7)
par(mfrow=c(1,2))
plot(fitted(fit7),residuals(fit7),xlab="Fitted", ylab="Residuals")
qqnorm(residuals(fit7),ylab="Residuals")
qqline(residuals(fit7))
hist(residuals(fit7))
plot(fit1$residuals)
names(xlevels)
1-sum(residuals(fit7)^2)/sum((Boston$medv-mean(Boston$medv))^2)
dwtest(fit7$model)

#import data
install.packages('Sleuth2')
library(Sleuth2)
View(ex2224)
dat<-ex2224
# fit with poisson regression
fit1<-glm(Failures~System+Operator+Valve+Size+Mode,offset=log(Time),data=dat,family='poisson')
summary(fit1)
exp(fit1$coefficient)
#anova table
anova(fit1,test='Chi')
#Deviance Goodness of Fit test:
pchisq(fit1$deviance, df=fit1$df.residual, lower.tail=FALSE)

#Pearson Goodness of Fit test:
ssr <- sum(residuals(fit1, type="pearson")^2)
pchisq(ssr,df=fit1$df.residual, lower.tail=FALSE)

#use glmnet
library(glmnet)
x<-model.matrix(Failures~System+Operator+Valve+Size+Mode,data=dat)[,-1]
y<-dat$Failures
cv.fit<-cv.glmnet(x,y,family='poisson',offset=log(dat$Time),alpha=0)
fit2<-glmnet(x,y,family='poisson',offset=log(dat$Time),lambda=cv.fit$lambda.min,alpha=0)
coef(fit2)
fit3<-glmnet(x,y,family='poisson',offset=log(dat$Time),lambda=0)
coef(fit3)
coef(fit1)

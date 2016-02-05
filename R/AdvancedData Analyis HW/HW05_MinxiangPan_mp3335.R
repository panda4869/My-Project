#read data
library(MASS)
data(Boston)
library(car)
install.packages("fmsb")
library(fmsb)
fit1<-lm(medv~1+crim+zn+indus+nox+rm+age+tax,data=Boston)
vif(fit1)
kappa(Boston[c('crim', 'zn', 'indus', 'nox', 'rm', 'age', 'tax')])
library(glmnet)
x1=model.matrix(medv~crim+zn+indus+nox+rm+age+tax,data=Boston)[,-1]
y1=Boston$medv
# fit ridge linear regression
fit1.ridge<-glmnet(x=x1,y=y1,alpha=0)
plot(fit1.ridge)
cv.fit1.ridge<-cv.glmnet(x=x1,y=y1,alpha=0)
fit1.ridge.opt<-glmnet(x=x1,y=y1,alpha=0,lambda=cv.fit1.ridge$lambda.min)
coef(fit1.ridge.opt)
plot(cv.fit1.ridge)
#fit lasso linear regression
fit1.lasso<-glmnet(x=x1,y=y1,alpha=1)
plot(fit1.lasso)
cv.fit1.lasso<-cv.glmnet(x=x1,y=y1,alpha=1)
fit1.lasso.opt<-glmnet(x=x1,y=y1,alpha=1,lambda=cv.fit1.lasso$lambda.min)
coef(fit1.lasso.opt)
plot(cv.fit1.lasso)
install.packages("ridge")
library(ridge)
fit3<-linearRidge(formula=medv~1+crim+zn+indus+nox+rm+age+tax,data=Boston,lambda="automatic")
summary(fit3)

# load the package
library(lars)
install.packages("lars")
fit1.lasso<- lars(x=x1, y=y1, type="lasso")
# summarize the fit
summary(fit1.lasso)
# select a step with a minimum error
best_step <- fit1.lasso$df[which.min(fit1.lasso$RSS)]
coef(fit1.lasso)[best_step,]

fit1.stepwise<-stepAIC(fit1,direction=c("both"))
summary(fit1.stepwise)
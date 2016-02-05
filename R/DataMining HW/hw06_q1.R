#############################
# < Your Name Here：Minxiang Pan >
# STAT W4240 
# Homework 05
# < Homework Due Date >
#
#############################
#################
# Question 1 (a)
#################
library(ISLR)
attach(Hitters)
#remove observations for whose salary information is unknown
length(which(is.na(Hitters$Salary)))
Hitters1<-Hitters[-which(is.na(Salary)),]
length(which(is.na(Hitters1$Salary)))
detach(Hitters)
#log-transform the salaries
Hitters1$Salary<-log(Hitters1$Salary)
#################
# Question 1 (b)
#################
#Create a training set consisting of the first 200 observations
hitters.train<-Hitters1[1:200,]
#The test set consisting of the remaining observations
hitters.test<-Hitters1[-(1:200),]
#################
# Question 1 (c)
#################
# install the package 
install.packages("gbm")
library(survival)
library(splines)
library(gbm)
# use boosting to compute training error
set.seed (1)
lambda=seq(0.01,1,by=0.05)
train.mse=vector()
for (i in 1:length(lambda))
{
  boost.hitters=gbm(hitters.train$Salary~.,data=hitters.train,distribution="gaussian",n.tree=1000,shrinkage=lambda[i])
  trian.pred=predict(boost.hitters,data=hitters.train,n.trees=1000)
  train.mse[i]=mean((hitters.train$Salary-trian.pred)^2)
}
#plot the lambda against train mse
plot(lambda,train.mse,type="b",xlab="Lambda",ylab="Train MSE")
#################
# Question 1 (d)
#################
# use boosting to compute training error
set.seed (1)
lambda=seq(0.01,1,by=0.05)
test.mse=vector()
for (i in 1:length(lambda))
{
  boost.hitters=gbm(hitters.train$Salary~.,data=hitters.train,distribution="gaussian",n.tree=1000,shrinkage=lambda[i])
  test.pred=predict(boost.hitters,hitters.test,n.trees=1000)
  test.mse[i]=mean((hitters.test$Salary-test.pred)^2)
}
#plot the lambda against test mse
plot(lambda,test.mse,type="b",xlab="Lambda",ylab="Test MSE")
min(test.mse)
#################
# Question 1 (e)
#################
library(glmnet)
# use linear regression
lm.hitters<-lm(Salary~.,hitters.train)
lm.pred=predict(lm.hitters,hitters.test)
#compute the test MSE
mean((hitters.test$Salary-lm.pred)^2)
#use ridge for regression
set.seed(1)
#we have to transform the factor variables to dummy variables and transform the data into matrix 
x=model.matrix(Salary~.,data=hitters.train)
t=model.matrix(Salary~.,data=hitters.test)
y=hitters.train$Salary
cv.ridge.fit=cv.glmnet(x,y,alpha=0)
ridge.pred=predict(cv.ridge.fit,newx=t,s="lambda.min",type="class")
#MSE for ridge
mean((hitters.test$Salary-ridge.pred)^2)
#use lasso for regression
cv.lasso.fit=cv.glmnet(x,y,alpha=1)
lasso.pred=predict(cv.lasso.fit,newx=t,s="lambda.min",type="class")
#MSE for lasso
mean((hitters.test$Salary-lasso.pred)^2)
#################
# Question 1 (f)
#################
#find the best lambda
lambda.best=lambda[which(test.mse==min(test.mse))]
boost.hitters.best=gbm(hitters.train$Salary~.,data=hitters.train,distribution="gaussian",n.tree=1000,shrinkage=lambda.best)
summary(boost.hitters.best)
#################
# Question 1 (g)
#################
install.packages("randomForest")
#use bagging
library(randomForest)
set.seed (1)
# number of covariate
p=dim(hitters.train)[2]-1
#bagging is a special case for random forest
hitters.bag=randomForest(Salary~.,data=hitters.train,mtry=p,ntree=1000,importance =TRUE)
hitters.bag.pred=predict(hitters.bag,newdata=hitters.test)
#compute MSE
mean((hitters.test$Salary-hitters.bag.pred)^2)
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
#################
# Question 2 (a)
#################
#make a data frame
d=data.frame(x1=c(3,2,4,1,2,4,4),x2=c(4,2,4,4,1,3,1),Color=c("red", "red", "red", "red", "blue", "blue", "blue"))
#plot the observation
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5))
#################
# Question 2 (b)
#################
#draw the hyperplane
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5));abline(-0.5,1)
legend("topleft",c("hyperplane"),lty=c(1),bty="n",cex=0.9)
#################
# Question 2 (d)
#################
#plot the margin
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5));abline(-0.5,1)
k=1
b1=4-4
b2=3-4
#use the solid line to indicate the hyperplane and use dash lines to indicate the margin
abline(-0.5, k)
abline(b1, k, lty = 2)
abline(b2, k, lty = 2)
legend("topleft",c("hyperplane","margin"),lty=c(1,2),bty="n",cex=0.9)
#################
# Question 2 (e)
#################
#use the solid line to indicate the hyperplane and use dash lines to indicate the margin
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5));abline(-0.5,1)
abline(-0.5, k)
abline(b1, k, lty = 2)
abline(b2, k, lty = 2)
# use arrows to indicate support vectors
arrows(4,4,4.25,3.75);arrows(2,2,2.25,1.75);arrows(2,1,1.75,1.25);arrows(4,3,3.75,3.25)
legend("topleft",c("hyperplane","margin"),lty=c(1,2),bty="n",cex=0.9)
#################
# Question 2 (g)
#################
# we change the slope
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5));abline(-0.5,1.1)
legend("topleft",c("hyperplane"),lty=c(1),bty="n",cex=0.9)
#################
# Question 2 (h)
#################
#add an observation
d1=data.frame(x1=c(3,2,4,1,2,4,4,3),x2=c(4,2,4,4,1,3,1,1),Color=c("red", "red", "red", "red", "blue", "blue", "blue","red"))
#plot the observations
plot(d1$x1, d1$x2, col=as.vector(d1$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5))
#################
# Question 4 (a)
#################
# as the previous homework we center and scale the training data and the test data
#The first way is the majority vote approach:
# we cited the code in the HW04 to create the new traing sets and test sets
# This code uses tm to preprocess the papers into a format useful for NB
preprocess.directory = function(dirname){
  
  # the directory must have all the relevant text files
  ds = DirSource(dirname)
  # Corpus will make a tm document corpus from this directory
  fp = Corpus( ds )
  # inspect to verify
  # inspect(fp[1])
  # another useful command
  # identical(fp[[1]], fp[["Federalist01.txt"]])
  # now let us iterate through and clean this up using tm functionality
  # make all words lower case
  fp = tm_map( fp , content_transformer(tolower));
  # remove all punctuation
  fp = tm_map( fp, removePunctuation);
  # remove stopwords like the, a, and so on. 
  fp = tm_map( fp, removeWords, stopwords("english"));
  # remove stems like suffixes
  fp = tm_map( fp, stemDocument)
  # remove extra whitespace
  fp = tm_map( fp, stripWhitespace)
  # now write the corpus out to the files for our future use.
  # MAKE SURE THE _CLEAN DIRECTORY EXISTS
  writeCorpus( fp , sprintf('%s_clean',dirname), filenames = names(fp))
}
preprocess.directory('fp_hamilton_test')
preprocess.directory('fp_hamilton_train')
preprocess.directory('fp_madison_test')
preprocess.directory('fp_madison_train')
# To read in data from the directories:
# Partially based on code from C. Shalizi
read.directory <- function(dirname) {
  # Store the infiles in a list
  infiles = list();
  # Get a list of filenames in the directory
  filenames = dir(dirname,full.names=TRUE);
  for (i in 1:length(filenames)){
    infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
  }
  return(infiles)
}
hamilton.train <- read.directory('fp_hamilton_train_clean')
hamilton.test <- read.directory('fp_hamilton_test_clean')
madison.train <- read.directory('fp_madison_train_clean')
madison.test <- read.directory('fp_madison_test_clean')
# Make dictionary sorted by number of times a word appears in corpus 
# (useful for using commonly appearing words as factors)
# NOTE: Use the *entire* corpus: training, testing, spam and ham
make.sorted.dictionary.df <- function(infiles){
  # This returns a dataframe that is sorted by the number of times 
  # a word appears
  
  # List of vectors to one big vetor
  dictionary.full <- unlist(infiles) 
  # Tabulates the full dictionary
  tabulate.dic <- tabulate(factor(dictionary.full)) 
  # Find unique values
  dictionary <- unique(dictionary.full) 
  # Sort them alphabetically
  dictionary <- sort(dictionary)
  dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
  sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
  return(sort.dictionary.df)
}

doc.all <- c(hamilton.train,hamilton.test,madison.train,madison.test)
dictionary <- make.sorted.dictionary.df(doc.all)
# Make a document-term matrix, which counts the number of times each 
# dictionary element is used in a document
make.document.term.matrix <- function(infiles,dictionary){
  # This takes the text and dictionary objects from above and outputs a 
  # document term matrix
  num.infiles <- length(infiles);
  num.words <- nrow(dictionary);
  # Instantiate a matrix where rows are documents and columns are words
  dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
  for (i in 1:num.infiles){
    num.words.infile <- length(infiles[[i]]);
    infile.temp <- infiles[[i]];
    for (j in 1:num.words.infile){
      ind <- which(dictionary == infile.temp[j])[[1]];
      # print(sprintf('%s,%s', i , ind))
      dtm[i,ind] <- dtm[i,ind] + 1;
      #print(c(i,j))
    }
  }
  return(dtm);
}

dtm.hamilton.train <- make.document.term.matrix(hamilton.train,dictionary)
dtm.hamilton.test <- make.document.term.matrix(hamilton.test,dictionary)
dtm.madison.train <- make.document.term.matrix(madison.train,dictionary)
dtm.madison.test <- make.document.term.matrix(madison.test,dictionary)

##################
# creat two new dataframe for training set and test set
train.frame=as.data.frame(rbind(dtm.hamilton.train,dtm.madison.train ))
names(train.frame)=as.vector(dictionary$word)
test.frame=as.data.frame(rbind(dtm.hamilton.test,dtm.madison.test ))
names(test.frame)=as.vector(dictionary$word)
test.list=train.list=vector()
train.list[1:35]=1
train.list[36:50]=0
test.list[1:16]=1
test.list[17:27]=0
train.list=as.factor(train.list)
test.list=as.factor(test.list)
train.frame=cbind(train.frame,data.frame(Author=train.list))
test.frame=cbind(test.frame,data.frame(Author=test.list))
# we scale the training data first
x=scale(train.frame[,-length(train.frame)])
x[is.na(x)]=0
y=train.frame$Author
# we use train mean and train sd to scale the testing data
# we cite the code in the hw05 solution
train.mean=apply(train.frame[,-length(train.frame)],2,mean)
train.sd=apply(train.frame[,-length(train.frame)],2,sd)
t=scale(test.frame[,-length(test.frame)],center=train.mean,scale=train.sd)
t[is.na(t)]=0
#set up 
library(e1071)
#train the model
tune.out=tune(svm,Author~.,data=cbind(x[,1:100],data.frame(Author=y)),kernel="linear",type="C",scale=FALSE,ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
#make prediction
bestmod=tune.out$best.model
bestmod.pred=predict(bestmod,cbind(t[,1:100],data.frame(Author=test.frame$Author)))
#show the predicting result
table(bestmod.pred,test.frame$Author)
#compute the rate of correct classification 
mean(bestmod.pred==test.frame$Author)
#################
# Question 4 (b)
#################
#use loop to make predictions for times 
correction.rate=vector()
for(i in 1:20)
{
  tune.out=tune(svm,Author~.,data=cbind(x[,1:(5*i)],data.frame(Author=y)),kernel="linear",type="C",scale=FALSE,ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
  bestmod=tune.out$best.model
  bestmod.pred=predict(bestmod,cbind(t[,1:(5*i)],data.frame(Author=test.frame$Author)))
  correction.rate[i]=mean(bestmod.pred==test.frame$Author)
}
#plot the number of words against the rate of classification correction
n.words=seq(5,100,by=5)
plot(n.words,correction.rate,xlab="n.words",ylab="correction.rate",type="b")
points(which.max(correction.rate)*5,correction.rate[which.max(correction.rate)],col="red")
title("Number of words vs Correction rate")
# locate the plot
correction.rate[which.max(correction.rate)]
which.max(correction.rate)*5
#################
# Question 4 (c)
#################
# we use radial kernel in this part
correction.rate=vector()
for(i in 1:20)
{
  tune.out=tune(svm,Author~.,data=cbind(x[,1:(5*i)],data.frame(Author=y)),kernel="radial",type="C",scale=FALSE,ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)),gamma=c(0.5,1,2,3,4))
  bestmod=tune.out$best.model
  bestmod.pred=predict(bestmod,cbind(t[,1:(5*i)],data.frame(Author=test.frame$Author)))
  correction.rate[i]=mean(bestmod.pred==test.frame$Author)
}
#plot the number of words against the rate of classification correction
n.words=seq(5,100,by=5)
plot(n.words,correction.rate,xlab="n.words",ylab="correction.rate",type="b")
points(which.max(correction.rate)*5,correction.rate[which.max(correction.rate)],col="red")
title("Number of words vs Correction rate")
# locate the plot
correction.rate[which.max(correction.rate)]
which.max(correction.rate)*5
#################
# Question 4 (d)
#################
#find the corresponding columns of the two important words
x1=cbind(x[,which(colnames(train.frame)=="depart")],x[,which(colnames(train.frame)=="upon")])
t1=cbind(t[,which(colnames(test.frame)=="depart")],t[,which(colnames(test.frame)=="upon")])
tune.out=tune(svm,Author~.,data=cbind(x1,data.frame(Author=y)),kernel="radial",type="C",scale=FALSE,ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)),gamma=c(0.5,1,2,3,4))
bestmod=tune.out$best.model
bestmod.pred=predict(bestmod,cbind(t1,data.frame(Author=test.frame$Author)))
#plot the result
table(bestmod.pred,test.frame$Author)
mean(bestmod.pred==test.frame$Author)

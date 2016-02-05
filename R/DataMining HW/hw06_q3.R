#############################
# < Your Name Here：Minxiang Pan >
# STAT W4240 
# Homework 05
# < Homework Due Date >
#
#############################
#################
# Question 3 (b)
#################
#we use the formula in part 3(a) to compute the mutual information
hamilton.p=35/50
madison.p=15/50
n.word=sum(rbind(dtm.hamilton.train,dtm.madison.train))
n.word1=sum(dtm.hamilton.train)
n.word2=sum(dtm.madison.train)
test.hamilton.p=apply(dtm.hamilton.train,2,sum)/n.word1
test.madison.p=apply(dtm.madison.train,2,sum)/n.word2
test.p=apply(rbind(dtm.hamilton.train,dtm.madison.train),2,sum)/n.word
Info.mutual.hamilton=hamilton.p*(test.hamilton.p*log(test.hamilton.p/test.p)+(1-test.hamilton.p)*log((1-test.hamilton.p)/(1-test.p)))
Info.mutual.madison=madison.p*(test.madison.p*log(test.madison.p/test.p)+(1-test.madison.p)*log((1-test.madison.p)/(1-test.p)))
Info.mutual=Info.mutual.hamilton+Info.mutual.madison
#create a new data frame to sort by mutual information
d.info=data.frame(Info=Info.mutual,words=as.vector(dictionary$word))
d.info[is.na(d.info)]=0
d.info.sorted=d.info[order(d.info[,1],decreasing=T),]
fix(d.info.sorted)
#sort the original training set and testing set by the important words
train.frame1=train.frame[,-length(train.frame)]
d.sorted.train.frame=train.frame1[,as.vector(d.info.sorted[,2])]
d.sorted.train.frame=cbind(d.sorted.train.frame,data.frame(Author=train.list))
test.frame1=test.frame[,-length(test.frame)]
d.sorted.test.frame=test.frame1[,as.vector(d.info.sorted[,2])]
d.sorted.test.frame=cbind(d.sorted.test.frame,data.frame(Author=test.list))
#use tree
#begin with Gini
# use package "rpart"
install.packages("rpart")
library(rpart)
#create a data frame to store the result
test.summary=data.frame("200"=c(0,0,0),"500"=c(0,0,0),"1000"=c(0,0,0),"2500"=c(0,0,0))
rownames(test.summary)<-c("Correct_rate","False_negative_rate","False_positive_rate")
mu=c(200,500,1000,2500)

for(i in 1:4)
{
  d.sorted.train.frame.loop=cbind(d.sorted.train.frame[,1:mu[i]],data.frame(Author=train.list))
  d.sorted.test.frame.loop=cbind(d.sorted.test.frame[,1:mu[i]],data.frame(Author=test.list))
  fit.train=rpart(d.sorted.train.frame.loop$Author~.,d.sorted.train.frame.loop,method="class")
  author.predGini=predict(fit.train,d.sorted.test.frame.loop,type="class")
  # correct classification rate
  test.summary[1,i]=mean(author.predGini==test.frame$Author)
  #false negative rate
  test.summary[2,i]=length(which(author.predGini!=test.frame$Author & author.predGini==0))/length(which(test.frame$Author==1))
  #false positive rate
  test.summary[3,i]=length(which(author.predGini!=test.frame$Author & author.predGini==1))/length(which(test.frame$Author==0))
}
fix(test.summary)
# plot the result
par(mfrow=c(3,1))
plot(mu,test.summary[1,],xlab="number of important words",ylab="Correction Rate",type="b")
title("number of important words vs Correction Rate")
plot(mu,test.summary[2,],xlab="number of important words",ylab="False Negative Rate",type="b")
title("number of important words vs False Negative Rate")
plot(mu,test.summary[3,],xlab="number of important words",ylab="False Positive Rate",type="b")
title("number of important words vs False Positive Rate")

# use the information gain to split the tree

for(i in 1:4)
{
  d.sorted.train.frame.loop=cbind(d.sorted.train.frame[,1:mu[i]],data.frame(Author=train.list))
  d.sorted.test.frame.loop=cbind(d.sorted.test.frame[,1:mu[i]],data.frame(Author=test.list))
  fit.train1=rpart(d.sorted.train.frame.loop$Author~.,d.sorted.train.frame.loop,method="class",parms=list(split='information'))
  author.predInfo=predict(fit.train1,d.sorted.test.frame.loop,type="class")
  # correct classification rate
  test.summary[1,i]=mean(author.predInfo==test.frame$Author)
  #false negative rate
  test.summary[2,i]=length(which(author.predInfo!=test.frame$Author & author.predInfo==0))/length(which(test.frame$Author==1))
  #false positive rate
  test.summary[3,i]=length(which(author.predInfo!=test.frame$Author & author.predInfo==1))/length(which(test.frame$Author==0))
}
fix(test.summary)
# plot the result
par(mfrow=c(3,1))
plot(mu,test.summary[1,],xlab="number of important words",ylab="Correction Rate",type="b")
title("number of important words vs Correction Rate")
plot(mu,test.summary[2,],xlab="number of important words",ylab="False Negative Rate",type="b")
title("number of important words vs False Negative Rate")
plot(mu,test.summary[3,],xlab="number of important words",ylab="False Positive Rate",type="b")
title("number of important words vs False Positive Rate")



#we scale the data
x=d.sorted.train.frame[,-length(d.sorted.train.frame)]
x.mean=apply(x,2,mean)
x.sd=apply(x,2,sd)
x=scale(x,center=T,scale=T)
x[is.na(x)]=0
y=d.sorted.train.frame$Author
t=d.sorted.test.frame[,-length(d.sorted.test.frame)]
t=scale(t,center=x.mean,scale=x.sd)
t[is.na(t)]=0
# use ridge for logistic regression
for(i in 1:4)
{
  x.loop=x[,1:mu[i]]
  t.loop=t[,1:mu[i]]
  cv.fit.ridge=cv.glmnet(x.loop,y,family="binomial",alpha=0,standardize=FALSE)
  cv.ridge.pred=predict(cv.fit.ridge,newx=t.loop,s="lambda.min",type="class")
  test.summary[1,i]=mean(cv.ridge.pred==test.frame$Author)
  #false negative rate
  test.summary[2,i]=length(which(cv.ridge.pred!=test.frame$Author & cv.ridge.pred==0))/length(which(test.frame$Author==1))
  #false positive rate
  test.summary[3,i]=length(which(cv.ridge.pred!=test.frame$Author & cv.ridge.pred==1))/length(which(test.frame$Author==0))   
}

fix(test.summary)
# plot the result
par(mfrow=c(3,1))
plot(mu,test.summary[1,],xlab="number of important words",ylab="Correction Rate",type="b")
title("number of important words vs Correction Rate")
plot(mu,test.summary[2,],xlab="number of important words",ylab="False Negative Rate",type="b")
title("number of important words vs False Negative Rate")
plot(mu,test.summary[3,],xlab="number of important words",ylab="False Positive Rate",type="b")
title("number of important words vs False Positive Rate")

#use lasso for logistic regression

for(i in 1:4)
{
  x.loop=x[,1:mu[i]]
  t.loop=t[,1:mu[i]]
  cv.fit.lasso=cv.glmnet(x.loop,y,family="binomial",alpha=1,standardize=FALSE)
  
  cv.lasso.pred=predict(cv.fit.lasso,newx=t.loop,s="lambda.min",type="class")
  test.summary[1,i]=mean(cv.lasso.pred==test.frame$Author)
  #false negative rate
  test.summary[2,i]=length(which(cv.lasso.pred!=test.frame$Author & cv.lasso.pred==0))/length(which(test.frame$Author==1))
  #false positive rate
  test.summary[3,i]=length(which(cv.lasso.pred!=test.frame$Author & cv.lasso.pred==1))/length(which(test.frame$Author==0))  
}
fix(test.summary)
# plot the result
par(mfrow=c(3,1))
plot(mu,test.summary[1,],xlab="number of important words",ylab="Correction Rate",type="b")
title("number of important words vs Correction Rate")
plot(mu,test.summary[2,],xlab="number of important words",ylab="False Negative Rate",type="b")
title("number of important words vs False Negative Rate")
plot(mu,test.summary[3,],xlab="number of important words",ylab="False Positive Rate",type="b")
title("number of important words vs False Positive Rate")
#Problem 1 #
#Q1#
classify<-function(S,z)
{
  y<-sign(S%*%z)
  return(y)
  
}
#Q2#
#fakedata#
#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size

#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels

fakedata <- function(w, n){
  
  if(! require(MASS))
  {
    install.packages("MASS")
  }
  if(! require(mvtnorm))
  {
    install.packages("mvtnorm")
  }
  
  require(MASS)
  require(mvtnorm)
  
  # obtain dimension
  d <- length(w)-1
  
  # compute the offset vector and a Basis consisting of w and its nullspace
  offset <- -w[length(w)] * w[1:d] / sum(w[1:d]^2)
  Basis <- cbind(Null(w[1:d]), w[1:d])	 
  
  # Create samples, correct for offset, and extend
  # rmvnorm(n,mean,sigme) ~ generate n samples from N(0,I) distribution
  S <- rmvnorm(n, mean=rep(0,d),sigma = diag(1,d)) %*%  t(Basis) 
  S <- S + matrix(rep(offset,n),n,d,byrow=T)
  S <- cbind(S,1)
  
  # compute the class assignments
  y <- as.vector(sign(S %*% w))
  
  # add corrective factors to points that lie on the hyperplane.
  S[y==0,1:d] <- S[y==0,1:d] + runif(1,-0.5,0.5)*10^(-4)
  y = as.vector(sign(S %*% w))
  return(list(S=S, y=y))

#preceptrain fuction#  
#S: n by (d+1) sample matrix with last col 1  generated by the fake data#
#y is the original label of the training sets#
#z is the initial hyperplane#
preceptrain<-function(S,y,z){
  
  c<-classify(S,z)
  cp<-sum(ifelse(c!=y,1,0)*abs(S%*%z))#calculate the cost function#
  k=1
  #create z_history to save the interim hyperplanes#
  Z_history=matrix(rep(0,dim(S)[2]),ncol=dim(S)[2])
  Z_history[1,]=z
  #using gradient algorithm to find the best hyperplane#
  while(cp!=0)
  {
    delta.cp<-apply(matrix(rep(ifelse(c!=y,1,0)*(-y),dim(S)[1]*dim(S)[2]),dim(S)[1],dim(S)[2],byrow=F)*S,2,sum)
    z<-z-delta.cp/k
    Z_history<-rbind(Z_history,z)
    c<-classify(S,z)
    cp<-sum(ifelse(c!=y,1,0)*abs(S%*%z))
    k=k+1
  }
  return(list(z=z,z_history=Z_history))
}
#input a 3D hyperplane to test our algorithm#
set.seed(1)  
f.data<-fakedata(c(-1,1,1,-1),4) 
result<-preceptrain(f.data$S,f.data$y,c(1,2,1,-2))

#Q3#
#randomly inputs a hyperplane to generate the data#
#train the data#
set.seed(1)  
f.data<-fakedata(c(-1,1,-1),100) 
result<-preceptrain(f.data$S,f.data$y,c(1,2,-4))
c1<-classify(f.data$S,result$z)
sum(c1!=f.data$y)
#generate another set of linear saparable data #
set.seed(2)  
f.data<-fakedata(c(-1,1,-1),100) 
c2<-classify(f.data$S,result$z)
#show the error rate#
error.rate<-sum(c2!=f.data$y)/length(c)
error.rate
table(c2,f.data$y)

#Q4#
#create a function for drawing a hyperplane#
#the return is a list of vector standing for two end points for the line#
hyperplane<-function(z){
  #calculate the vector vertical to the VH#
  vertical<-Null(z[1:(length(z)-1)])
  #calculating the offset#
  c<--z[length(z)]*z[1:(length(z)-1)]/sum(z[1:(length(z)-1)]^2)
  #calculate the slope#
  k<-vertical[2]/vertical[1]
  #calculate the points#
  x1<--3+c[1]
  x2<-3+c[1]
  y1<--3*k+c[2]
  y2<-3*k+c[2]
  return (list(x=c(x1,x2),y=c(y1,y2)))
}

plot(f.data$S,xlim=c(-5,5),ylim=c(-5,5),col=ifelse(f.data$y==1,"blue","red"),cex=0.5,xlab="x1",ylab="x2")
#use loop to draw all the z_history#
for(i in 1:dim(result$z_history)[1]){
  data<-hyperplane(result$z_history[i,])
  if(i==1)
  lines(data$x,data$y,xlim=c(-5,-5),ylim=c(-5,5),col="green",lwd=0.5)
  if(i==length(result$z_history))
    lines(data$x,data$y,xlim=c(-5,-5),ylim=c(-5,5),col="blue",lwd=0.5)
  if(i>1 & i<length(result$z_history))
    lines(data$x,data$y,xlim=c(-5,-5),ylim=c(-5,5),col="black",lwd=0.5)
}
title("Gradient algorithm")

#Problem 3(a)#
library(e1071)
#read data#
data<-read.table("uspsdata.txt")
label1<-read.table("uspscl.txt")
set.seed(1)
num<-sample.int(200,160)
data1<-data[num,]
label<-label1[num,]
#create linear SVM#
linear.svm<-function(train.data,train.label,cost,k){
  folds<-dim(train.data)[1]/k
  max.train_size<-dim(train.data)[1]
  error.rate<-matrix(rep(0,length(cost)*1),length(cost),1)
  #use loop to do crossvalidation#
  for(j in 1:length(cost)){
    error<-0
  for(i in 1:k){
  #saperate training data into training set and validation set#  
  ind.test<-c(1: max.train_size)[(folds*(i-1)+1):(folds*i)]
  ind.train<-c(1: max.train_size)[-ind.test]
  #use the linear kernel#
  svm.train<-svm(train.data[ind.train,],train.label[ind.train],type='C',kernel='linear',cost=cost[j],gamma=0)
  svm.pred<-predict(svm.train,train.data[ind.test,])
  #sum up the times of error #
  error=error+sum(svm.pred!=train.label[ind.test])
  }
  #calculating the error rate#
  error.rate[j,]=error/max.train_size
}
#save the result and return#
result<-data.frame(C=cost,Error=error.rate)
return(result)
}
cost<-2^seq(-10,0,0.5)
result<-linear.svm(data1,label,cost,5)
#plot the picture#
plot(result$C,result$Error,xlim=c(0,1),type="l",xlab="cost",ylab="error_rate")
title("linear kernal")

#Problem 3(b)#
#create a radial kernel#
radial.svm<-function(train.data,train.label,cost,k,g){
  folds<-dim(train.data)[1]/k
  max.train_size<-dim(train.data)[1]
  error.rate<-matrix(rep(0,length(cost)*length(g)),length(g),length(cost))
  #use three loops to do the CV cost and gamma are variables#
  for(l in 1:length(g)){
  
  for(j in 1:length(cost)){
    error<-0
    for(i in 1:k){
      
      ind.test<-c(1: max.train_size)[(folds*(i-1)+1):(folds*i)]
      ind.train<-c(1: max.train_size)[-ind.test]
      svm.train<-svm(train.data[ind.train,],train.label[ind.train],type='C',kernel='radial',cost=cost[j],gamma=g[l])
      svm.pred<-predict(svm.train,train.data[ind.test,])
      error=error+sum(svm.pred!=train.label[ind.test])
    }
    error.rate[l,j]=error/max.train_size
  } 

}
#return the error rate#
return(error.rate)
}

cost<-2^seq(-10,0,0.5)
g=c(0.0001,0.001,0.01,0.1)
error<-radial.svm(data1,label,cost,5,g)
#use loop to draw all the lines under each gamma#
for(i in 1:dim(error)[1]){
  if(i==1) plot(cost,error[i,],type="l",ylim=c(0,1),xlab="cost",ylab="error_rate")
  if(i==2) lines(cost,error[i,],lty=2,col="red")
  if(i==3) lines(cost,error[i,],lty=2,col="blue")
  if(i==4) lines(cost,error[i,],lty=2,col="green")
  
}
#draw the discribtion#
legend("topleft",c("gamma=10^(-4)","gamma=10^(-3)","gamma=10^(-2)","gamma=10^(-1)"),lty=c(1,2,2,2),col=c("black","red","blue","green"),bty="n")
title("radial kernel")

#Q3(2)#
#use the test set for the linear case#
data2<-data[-num,]
label2<-label1[-num,]
label<-as.fector(label)
svm.train<-svm(data1[1:dim(data1)[1],],label[1:length(label)],type='C',kernel='linear',cost=cost[3],gamma=0)
svm.pred<-predict(svm.train,data2[1:length(data2),])
e<-sum(svm.pred!=label2)
e
#use the test set for the radial kernel case#

svm.train<-svm(data1[1:dim(data1)[1],],label[1:length(label)],type='C',kernel='radial',cost=cost[16],gamma=g[2])
svm.pred<-predict(svm.train,data2[1:length(data2),])
e<-sum(svm.pred!=label2)
e

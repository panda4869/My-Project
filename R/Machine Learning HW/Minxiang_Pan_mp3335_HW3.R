#(1)#
#the main function for adaboosing#
AdaBoost<-function(X,y,iter){
  n<-dim(X)[1]
  w<-rep(1/n,n)
  pars<-rep(list(list()),iter)
  alpha<-vector()
  label<-vector()
  #train the data and change the weight of data each time#
  for(i in 1:iter){
    pars[[i]]<-train(X,w,y)
    label<-classify(X,pars[[i]])
    err<-sum(w[which(label!=y)])/sum(w)
#if error is zero, set all alpha to 1 no necessary to change weight and we've found the best classifier#
    if(err!=0){
      alpha[i]<-log((1-err)/err)
      w[which(label!=y)]<-w[which(label!=y)]*exp(alpha[i])
      
    }else{alpha[i]<-1}
  }
  return(list(alpha=alpha,allPars=pars))
}
#for prediction#
agg_calss<-function(X,alpha,allPars){
  c<-rep(0,dim(X)[1])
  for(i in 1:length(alpha)){
    c<-c+classify(X,allPars[[i]])*alpha[i] 
  }
  c_hat<-sign(c)
  return(c_hat)
}
#(2)#
#Train the data and choose a best weak learner#
train<-function(X,w,y){
  n<-dim(X)[1]
  d<-dim(X)[2]
  j<-rep(0,d)
  theta<-rep(0,d)
  m<-rep(0,d)
  temp<-vector()
  loss<-vector()
  y1<-vector()
  w1<-vector()
  id<-vector()
#use loop to find the best weak leaner under each dimension#
for(i in 1:d){
#sort the data in each dimension and calculate the weighted cumsum#
#  for(j in 1:n){
#    y1[j]<-y[which(X[,i]==sort(X[,i])[j])]
#    w1[j]<-w[which(X[,i]==sort(X[,i])[j])]
#    X1[j]<-X[which(X[,i]==sort(X[,i])[j])]
# } this method really bad very expensive to compute 
#use order instead#
  id<-order(X[,d])
  y1<-y[id]
  w1<-w[id]
  X1<-X[id,i]
  y_cum<-cumsum(y1*w1)
  #choose the first theta which have the maximum abs cumulative sum#
  threshold<-min(which(abs(y_cum)==max(abs(y_cum))))
  
  #find the mode #
  m[i]<-ifelse(y_cum[threshold]<0,-1,+1)
  #store the theta#
  theta[i]<-X1[threshold]
  #decision stump#
  label<-ifelse(X[,i]>theta[i],-m[i],m[i])
  #calculate the weighted loss#
  loss[i]<-sum(w*(label!=y))/sum(w)
}
#find the minimun loss#
  opt<-which.min(loss)
  return(list(j=opt,theta=theta[opt],mod=m[opt]))
}
#classify#
classify<-function(X,pars){
  label<-vector()
  label<-ifelse(X[,pars$j]>pars$theta,-pars$mod,pars$mod)
  return(label)
}

#(3)#

#cross validation to select the times of iteration#
x<-read.table("uspsdata.txt")
y<-read.table("uspscl.txt")
#transform the data into matrix and vector#
xdata<-mat.or.vec(dim(x)[1],dim(x)[2])
ydata<-vector()
for(i in 1: dim(x)[1]){
  ydata[i]<-y[i,1]
  for(j in 1:dim(x)[2]){
    xdata[i,j]<-x[i,j]
  }
}
#sample from the data#
n<-dim(xdata)[1]

#draw 80 percent of the data as the training sets as HW2#
set.seed(1)
num<-sample.int(n,round(n*0.8))
x1<-xdata[num,]
y1<-ydata[num]
#creat a CV function#
cross_validation<-function(x1,y1,k,max_iter){
folds<-dim(x1)[1]/k
max_train_size<-dim(x1)[1]
train_error_rate<-vector()
test_error_rate<-vector()
for (i in 1:max_iter){
  err1<-0
  err2<-0
  for(j in 1:k){
   ind.test<-c(1: max_train_size)[(folds*(j-1)+1):(folds*j)]
   ind.train<-c(1: max_train_size)[-ind.test]
  pars<-AdaBoost(x1[ind.train,],y1[ind.train],i)
  c1<-agg_calss(x1[ind.train,],pars$alpha,pars$allPars)
  c2<-agg_calss(x1[ind.test,],pars$alpha,pars$allPars)
  err1<-err1+mean(c1!=y1[ind.train])
  err2<-err2+mean(c2!=y1[ind.test])
  
  }
  train_error_rate[i]<-err1/k
  test_error_rate[i]<-err2/k
}
result<-data.frame(iteration=c(1:max_iter),Train_err=train_error_rate,Test_err=test_error_rate)
return(result)
}
#plot the result#
result<-cross_validation(x1,y1,5,60)
par(mfrow=c(1,2))
plot(result$iteration,result$Train_err,type="l",xlab="Number of iteration",ylab="Error_rate",main="Training error")
plot(result$iteration,result$Test_err,type="l",xlab="Number of iteration",ylab="Error_rate",main="Test error")

#6.7#

for (i in 1:1000){
  
  
  
  
  
}




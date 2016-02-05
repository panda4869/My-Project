#read data
train.dat<-read.csv('train.csv')
store.dat<-read.csv('store.csv')

train.merge<-merge(train.dat,store.dat,by.x=c('Store'),by.y=c('Store'))
colnames(train.dat)
colnames(store.dat)
View(train.merge)
#after we modify the features we fed the data into R
test_merged.dat<-read.csv('test_merged.csv')
View(test_merged.dat)

train_merged.dat<-read.csv('train_merged.csv')
View(train_merged.dat)
#using the features
features<-c('Store','Sales','CompetitionDistance','Promo','Promo2','SchoolHoliday','StoreType','Assortment','StateHoliday',
            'DayOfWeek','Month','Day','Year','WeekOfYear','CompetitionOpen','PromoOpen','IsPromoMonth')



train_model.dat<-train_merged.dat[,features]
set.seed(1234)
train_proportion<-sample(dim(train_model.dat)[1],0.8*nrow(train_model.dat))

train_model.dat1<-train_model.dat[train_proportion,]
train_model.dat2<-train_model.dat[-train_proportion,]

predictors<-c('CompetitionDistance','Promo','Promo2','SchoolHoliday','StoreType','Assortment','StateHoliday',
            'DayOfWeek','Month','Day','Year','WeekOfYear','CompetitionOpen','PromoOpen','IsPromoMonth')


train_model.dat1$Store <-as.numeric(train_model.dat1$Store)
train_model.dat1$Promo  <-as.numeric(train_model.dat1$Promo)
train_model.dat1$Promo2<-as.numeric(train_model.dat1$Promo2)
train_model.dat1$StoreType <-as.numeric(train_model.dat1$StoreType)
train_model.dat1$Sales <-as.numeric(train_model.dat1$Sales)
train_model.dat1$Assortment<-as.numeric(train_model.dat1$Assortment)
train_model.dat1$StateHoliday<-as.numeric(train_model.dat1$StateHoliday)
train_model.dat1$DayOfWeek<-as.numeric(train_model.dat1$DayOfWeek)
train_model.dat1$Month <-as.numeric(train_model.dat1$Month )
train_model.dat1$Day<-as.numeric(train_model.dat1$Day)
train_model.dat1$Year<-as.numeric(train_model.dat1$Year)

train_model.dat1$WeekOfYear<-as.numeric(train_model.dat1$WeekOfYear)
train_model.dat1$IsPromoMonth<-as.numeric(train_model.dat1$IsPromoMonth)




train_model.dat2$Store <-as.numeric(train_model.dat2$Store)
train_model.dat2Promo  <-as.numeric(train_model.dat2$Promo)
train_model.dat2Promo2<-as.numeric(train_model.dat2$Promo2)
train_model.dat2StoreType <-as.numeric(train_model.dat2$StoreType)
train_model.dat2Sales <-as.numeric(train_model.dat2$Sales)
train_model.dat2Assortment<-as.numeric(train_model.dat2$Assortment)
train_model.dat2StateHoliday<-as.numeric(train_model.dat2$StateHoliday)
train_model.dat2DayOfWeek<-as.numeric(train_model.dat2$DayOfWeek)
train_model.dat2Month <-as.numeric(train_model.dat2$Month )
train_model.dat2Day<-as.numeric(train_model.dat2$Day)
train_model.dat2Year<-as.numeric(train_model.dat2$Year)

train_model.dat2WeekOfYear<-as.numeric(train_model.dat2$WeekOfYear)
train_model.dat2IsPromoMonth<-as.numeric(train_model.dat2$IsPromoMonth)
#K fold CV
k=5
cv<-floor(nrow(train_model.dat1)/(k+1))

library(caret) # for dummyVars
library(RCurl) # download https data
library(Metrics) # calculate errors
library(xgboost) # model
smallesterr<-10000
dep=10
  for (round in seq(1,20,1)){
    totalerr<-vector()
    for (i in 1:10){
      test_index<-c((i-1)*cv+1:i*cv)
      test_x<-train_model.dat1[test_index,predictors]
      test_y<-train_model.dat1[test_index,"Sales"]
      test_x<-train_model.dat1[-test_index,predictors]
      test_y<-train_model.dat1[-test_index,"Sales"]
      gbm<-xgboost(data=as.matrix(test_x),label=test_y,max.depth=dep,nround=round,
                   objective="reg:linear",verbose=0)
      gc()
      predictions<-predict(gbm,as.matrix(test_x),outputmargin=TRUE)
      err<-rmse(test_y,predictions)
      totalerr<-c(totalerr,err)      
    }
    
    if(mean(totalerr)<smallesterr){
      smallesterr=mean(totalerr)
      small_dep=dep
      small_round=round
      
      
    }
    
    
    
  } 
  


print(paste(small_dep,small_round,smallesterr))

gbm<-xgboost(data=as.matrix(train_model.dat1[,predictors]),label=train_model.dat1[,"Sales"],max.depth=10,nround=20,
             objective="reg:linear",verbose=0)

predictions<-predict(gbm,as.matrix(train_model.dat2[,predictors]),outputmargin=TRUE)
err<-rmse(train_model.dat2[,"Sales"],predictions)
importance_matrix<-xgb.importance(predictors,model=gbm)
jpeg('rplot.jpg')
xgb.plot.importance(importance_matrix)
dev.off()

install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)

install.packages("Rcpp")


result<-data.frame(fit=predictions,y=train_model.dat2[,"Sales"],dif=predictions-train_model.dat2[,"Sales"])



t1<-train_model.dat2[train_model.dat2$Store==1,][1:42,]
t1.test<-t1[,predictors]
t1.label<-t1[,"Sales"]
predictions<-predict(gbm,as.matrix(t1.test),outputmargin=TRUE)
err_mean<-mean(abs(predictions-t1.label))
err.rate<-abs(predictions-t1.label)/t1.label
#grow a tree
library(rpart)
fit <- rpart(Sales~CompetitionDistance+Promo+Promo2+SchoolHoliday+StoreType+Assortment+StateHoliday+
             DayOfWeek+Month+Day+Year+WeekOfYear+CompetitionOpen+PromoOpen+IsPromoMonth,method="anova",
             data=t1,parms=c('information'), control = rpart.control(minsplit = 2))
par(mfrow=c(1,1))
plot(fit, uniform=TRUE, main="Regression Tree for Sales ")
text(fit, use.n=TRUE, all=TRUE, cex=.5)
plot(pfit, uniform=TRUE, main="Regression Tree for Sales ")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
pfit<-prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
summary(pfit)

predictions1<-predict(pfit,t1.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions1-t1.label))
err.rate.tree1<-sum(abs(predictions1-t1.label))/sum(t1.label)
quantile(err.rate,c(0.25,0.75))

plot(t1.label,type='l',lty=1)
lines(predictions1,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)


t2<-train_model.dat2[train_model.dat2$Store==2,][1:42,]
t2.test<-t2[,predictors]
t2.label<-t2[,"Sales"]

#tree
predictions2<-predict(pfit,t2.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions2-t2.label))
err.rate.tree2<-sum(abs(predictions2-t2.label))/sum(t2.label)
quantile(err.rate,c(0.25,0.75))
plot(t2.label,type='l',lty=1)
lines(predictions2,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)


t3<-train_model.dat2[train_model.dat2$Store==3,][1:42,]
t3.test<-t3[,predictors]
t3.label<-t3[,"Sales"]
#tree
predictions3<-predict(pfit,t3.test,outputmargin=TRUE)
err_mean.tree1<-mean(abs(predictions3-t3.label))
err.rate.tree3<-sum(abs(predictions3-t3.label))/sum(t3.label)
quantile(err.rate,c(0.25,0.75))
plot(t3.label,type='l',lty=1)
lines(predictions3,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)

t4<-train_model.dat2[train_model.dat2$Store==4,][1:42,]
t4.test<-t4[,predictors]
t4.label<-t4[,"Sales"]

#tree
predictions4<-predict(pfit,t4.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions4-t4.label))
err.rate.tree4<-sum(abs(predictions4-t4.label))/sum(t4.label)
quantile(err.rate,c(0.25,0.75))
plot(t4.label,type='l',lty=1)
lines(predictions4,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)


t5<-train_model.dat2[train_model.dat2$Store==5,][1:42,]
t5.test<-t5[,predictors]
t5.label<-t5[,"Sales"]
#tree
predictions5<-predict(pfit,t5.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions5-t5.label))
err.rate.tree5<-sum(abs(predictions5-t5.label))/sum(t5.label)
quantile(err.rate,c(0.25,0.75))
plot(t5.label,type='l',lty=1)
lines(predictions5,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)


t6<-train_model.dat2[train_model.dat2$Store==6,][1:42,]
t6.test<-t6[,predictors]
t6.label<-t6[,"Sales"]
#tree
predictions6<-predict(pfit,t6.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions6-t6.label))
err.rate.tree6<-sum(abs(predictions6-t6.label))/sum(t6.label)
quantile(err.rate,c(0.25,0.75))
plot(t6.label,type='l',lty=1)
lines(predictions6,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)

t7<-train_model.dat2[train_model.dat2$Store==7,][1:42,]
t7.test<-t7[,predictors]
t7.label<-t7[,"Sales"]
#tree
predictions7<-predict(pfit,t7.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions7-t7.label))
err.rate.tree7<-sum(abs(predictions7-t7.label))/sum(t7.label)
quantile(err.rate,c(0.25,0.75))
plot(t7.label,type='l',lty=1)
lines(predictions7,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)

t8<-train_model.dat2[train_model.dat2$Store==8,][1:42,]
t8.test<-t8[,predictors]
t8.label<-t8[,"Sales"]
#tree
predictions8<-predict(pfit,t8.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions8-t8.label))
err.rate.tree8<-sum(abs(predictions8-t8.label))/sum(t8.label)
quantile(err.rate,c(0.25,0.75))
plot(t8.label,type='l',lty=1)
lines(predictions8,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)


t9<-train_model.dat2[train_model.dat2$Store==9,][1:42,]
t9.test<-t9[,predictors]
t9.label<-t9[,"Sales"]
#tree
predictions9<-predict(pfit,t9.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions9-t9.label))
err.rate.tree9<-sum(abs(predictions9-t9.label))/sum(t9.label)
quantile(err.rate,c(0.25,0.75))
plot(t9.label,type='l',lty=1)
lines(predictions9,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)

t10<-train_model.dat2[train_model.dat2$Store==10,][1:42,]
t10.test<-t10[,predictors]
t10.label<-t10[,"Sales"]
predictions10<-predict(pfit,t10.test,outputmargin=TRUE)
err_mean<-mean(abs(predictions10-t10.label))
err.rate.tree10<-sum(abs(predictions10-t10.label))/sum(t10.label)
quantile(err.rate,c(0.25,0.75))
plot(t10.label,type='l',lty=1)
lines(predictions10,lty=2,col="red")
legend("topright",c("Real Value","Fitted Value"),lty=1:2)


a=c(err.rate.tree1,err.rate.tree2,err.rate.tree3,err.rate.tree4,err.rate.tree5
    ,err.rate.tree6,err.rate.tree7,err.rate.tree8,err.rate.tree9,err.rate.tree10)
par(mfrow=c(1,1))
plot(a,xlab="Store",ylab="Abs Error Rate",main="Error Rates",type='b',xaxt="n")
axis(1,at=seq(0,10,1),cex=1)
par(mfrow=c(1,2))

plot(t1.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 1")
lines(predictions1,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)
plot(t2.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 2")
lines(predictions2,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)

plot(t3.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 3")
lines(predictions3,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)
plot(t4.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 4")
lines(predictions4,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)

plot(t5.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 5")
lines(predictions5,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)
plot(t6.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 6")
lines(predictions6,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)

plot(t7.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 7")
lines(predictions7,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)
plot(t8.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 8")
lines(predictions8,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)

plot(t9.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 9")
lines(predictions9,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)
plot(t10.label,type='l',lty=1,xlab="Day",ylab="Sales",main="Store 10")
lines(predictions10,lty=2,col="red")
legend("topleft",c("Real Value","Fitted Value"),lty=1:2,col=c("black","red"),cex=.6)

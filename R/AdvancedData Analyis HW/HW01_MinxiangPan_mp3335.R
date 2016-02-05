#read data
dat<-read.csv("case0102.csv")
View(dat)
#inspect the missing value
length(which(is.na(dat)))
#inspect outlier
par(mfrow=c(1,1))
hist(dat$Salary,main="Histogram of the salary in general",prob=T)
lines(density(dat$Salary))
rug(jitter(dat$Salary))
boxplot(dat$Salary,main="Boxplot of the salary in general")
rug(jitter(dat$Salary),side=2)
abline(h=mean(dat$Salary,na.rm=T),lty=2)
#spot the outlier by click on the plot
plot(dat$Salary,main="Plot of Salary",ylim=c(3000,9000))
abline(h=mean(dat$Salary,na.rm=T),lty=2)
abline(h=mean(dat$Salary,na.rm=T)+3*sd(dat$Salary,na.rm=T),lty=2)
abline(h=mean(dat$Salary,na.rm=T)-3*sd(dat$Salary,na.rm=T),lty=2)
abline(h=median(dat$Salary,na.rm=T),lty=2)
click<-identify(dat$Salary)
dat[click,]
#split the data set
dat.m<-dat[dat$Sex=="Male",]
rownames(dat.m) <- NULL
dat.f<-dat[dat$Sex=="Female",]
rownames(dat.f) <- NULL
#conduct EDA analysis
#histogram by sex
histogram(~dat$Salary|Sex,data=dat,col="light blue",main="Histogram of Salary by sex",xlab="Salary")
#inspect normality
par(mfrow=c(1,2))
qqnorm(dat.m$Salary)
qqline(dat.m$Salary)
qqnorm(dat.f$Salary)
qqline(dat.f$Salary)
shapiro.test(dat.m$Salary)
shapiro.test(dat.m$Salary)
#Sample mean median and standard deviation
mean(dat.m$Salary)
mean(dat.f$Salary)
median(dat.m$Salary)
median(dat.f$Salary)
sd(dat.m$Salary)
sd(dat.f$Salary)
# IQR and range
IQR(dat.m$Salary,na.rm=T)
IQR(dat.f$Salary,na.rm=T)
range(dat.m$Salary,na.rm=T)
range(dat.f$Salary,na.rm=T)
#bootstrap
boot.Salary<-function(iter,data){
  boot.mean<-rep(NA,iter)
  boot.median<-rep(NA,iter)
  boot.sd<-rep(NA,iter)
  boot.iqr<-rep(NA,iter)
  
  for(i in 1:iter){
    boot.mean[i]<-mean(sample(data,length(data),replace=T))
    boot.median[i]<-median(sample(data,length(data),replace=T))
    boot.iqr[i]<-IQR(sample(data,length(data),replace=T),na.rm=T)
    boot.sd[i]<-sd(sample(data,length(data),replace=T))
  }
  return(data.frame(BootstrapMean=mean(boot.mean),BootstrapMedian=mean(boot.median),BootstrapSd=mean(boot.sd),BootstrapIQR=mean(boot.iqr)
                    ,Variance.sd=var(boot.sd),Variance.iqr=var(boot.iqr),Variance.mean=var(boot.mean),Variance.median=var(boot.median)))
  
}
#Male
test.m<-boot.Salary(1000,dat.m$Salary)

test.m$BootstrapMean-mean(dat.m$Salary)
test.m$Variance.mean
test.m$BootstrapMedian-median(dat.m$Salary)
test.m$Variance.median

test.m$BootstrapSd-sd(dat.m$Salary)
test.m$Variance.sd
test.m$BootstrapIQR-IQR(dat.m$Salary,na.rm=T)
test.m$Variance.iqr
#Female
test.f<-boot.Salary(1000,dat.f$Salary)
test.f$BootstrapMean-mean(dat.f$Salary)
test.f$Variance.mean
test.f$BootstrapMedian-median(dat.f$Salary)
test.f$Variance.median
test.f$BootstrapSd-sd(dat.f$Salary)
test.f$Variance.sd
test.f$BootstrapIQR-IQR(dat.f$Salary,na.rm=T)
test.f$Variance.iqr
#Jackknife
jackNife<-function(data,MEAN,MEDIAN,SD,IQR){
  d=length(data)
  p.mean=rep(NA,d)
  p.median=rep(NA,d)
  p.sd=rep(NA,d)
  p.iqr=rep(NA,d)
  for(i in 1: d){
    p.mean[i]=mean(data[-i])
    p.median[i]=median(data[-i])
    p.sd[i]=sd(data[-i])
    p.iqr[i]=IQR(data[-i])
  }
  
  
  return(data.frame(Bias.mean=(mean(p.mean)-MEAN)*(d-1),Var.mean=(d-1)*sum((p.mean-mean(p.mean))^2)/d,
                    Bias.median=(mean(p.median)-MEDIAN)*(d-1),Var.median=(d-1)*sum((p.median-mean(p.median))^2)/d,
                    Bias.sd=(mean(p.sd)-SD)*(d-1),Var.sd=(d-1)*sum((p.sd-mean(p.sd))^2)/d,
                    Bias.iqr=(mean(p.iqr)-IQR)*(d-1),Var.iqr=(d-1)*sum((p.iqr-mean(p.iqr))^2)/d))
}
#Male
result.m<-jackNife(dat.m$Salary,mean(dat.m$Salary),median(dat.m$Salary),sd(dat.m$Salary),IQR(dat.m$Salary))
result.m$Bias.mean
result.m$Var.mean
result.m$Bias.median
result.m$Var.median
result.m$Bias.sd
result.m$Var.sd
result.m$Bias.iqr
result.m$Var.iqr

result.f<-jackNife(dat.f$Salary,mean(dat.f$Salary),median(dat.f$Salary),sd(dat.f$Salary),IQR(dat.f$Salary))
result.f$Bias.mean
result.f$Var.mean
result.f$Bias.median
result.f$Var.median
result.f$Bias.sd
result.f$Var.sd
result.f$Bias.iqr
result.f$Var.iqr

install.packages("bootstrap")
library(bootstrap)
result<-jackknife(dat.m$Salary,function(x){median(x)})
names(result)
result$jack.bias
result$jack.se

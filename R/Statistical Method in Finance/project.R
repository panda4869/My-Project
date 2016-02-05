


library(tseries)
library(fGarch)
library(FinTS)

a=ts(scan("dowj.txt"),start=c(2001,1), frequency = 12)
a1<-log(a[2:length(a)]/a[1:length(a)-1])

a1<-ts(a1,start=c(2001,1))
#ADF test; test for stationary#
adf.test(a1)
a2<-diff(a1)
adf.test(a2)
#test the residual#
fit=lm(a2~1+time(a2))
r=resid(fit)
summary(fit)
par(mfrow=c(1,3))
pacf(r^2)
acf(r)
acf(r^2)
AutocorTest(r)  #残差是否存在序列相关
ArchTest(r)     #是否存在ARCH效应
fit1=garchFit(~arma(1,0)+garch(1,1), data=r, algorithm="nlminb+nm", trace=F, include.mean=F)
Box.test(fit1@residuals,lag=1,type="Ljung-Box")
LBQPlot(fit1@residuals)
summary(fit1)
condv1<-fit1@h.t

#load the data#
library(xlsx)
library(xlsxjars)
library(rJava)
dat<-read.xlsx(file.choose(),sheetName="A",head=FALSE)
colnames(dat)<-c("date","r")

garchModel<-function(data,predictionSize,trainingSize){
  k<-predictionSize
  l<-trainingSize
  condi.var<-vector()
  result<-NULL
for(i in 1:k){ 
  #upate the data#
  dat.train<-data$r[i:l+i-1]
  #estimate the trend#
  dat.train<-ts(dat.train)
  fit=lm(dat.train~1+time(dat.train))
  #eliminate the trend#
  r=resid(fit)
  #build model #
  fit1=garchFit(~arma(1,0)+garch(1,1), data=r, algorithm="nlminb+nm", trace=F, include.mean=F)
  #make the prediction#
  r2<-predict(fit1,n.ahead=1,mse="uncond")
  #record the test result#
  a<-adf.test(r)$p.value#adf#
  b<-AutocorTest(r)$p.value#AutocorTest#
  c<-ArchTest(r)$p.value#ArchTest#
  d<-Box.test(fit1@residuals,type="Ljung-Box")$p.value#Ljung-Box#
  #combine the result#
  result<-rbind(result,data.frame(Date=data$date[l+i],csd=r2$standardDeviation*sqrt(241),ADF=a,Autocorr=b,Arch=c/18,BoxLjung=d,row.names=NULL))
}
 return(result)
}
#make a prediction#
r<-garchModel(dat,40,160)

#save the result#
write.csv(r, file="result.csv")



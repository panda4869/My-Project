
#read data#
ashort<-read.table("ashort.tsm")
ashort<-ts(log(ashort))
ashort.dif<-ts(log(ashort))
#eliminate the trend and seasonality
ashort.dif<-diff(ashort)
#differencing#
ashort.dif<-diff(diff(ashort,lag=12))

#plot ACF,PACF#
op=par(mfrow=c(3,1))
plot(ashort.dif,type="o",pch=22,lty=1,pty=2,xlab="",main="Differenced Index")
abline(h=0)
acf(ashort.dif,lag.max=40,main="Differenced Index")
acf(ashort.dif,lag.max=40,type="partial",main="Differenced Index")
par(op)

#center the data #
as<-ts(ashort-mean(ashort))
#fit the model#
fit.arima<-arima(as,order=c(1,1,1),seasonal=list(order = c(0, 1, 1), period = 12),include.mean=FALSE,method="ML")
summary(fit.arima)
#compute CI#
se<-diag((fit.arima$var.coef)^0.5)
CI<-data.frame(lower=fit.arima$coef-1.96*se,upper=fit.arima$coef+1.96*se)
CI
#check the residual#

op=par(mfrow=c(3,1))
plot(fit.arima$residuals,type="o",pch=22,lty=1,pty=2,ylab="",xlab="year",main="Residuals from the fitted ARIMA(1,1,1)(0,1,1)[12] model")
abline(h=0)
acf(fit.arima$residuals,lag.max=40,main="")
acf(fit.arima$residuals,lag.max=40,type="partial",main="")
par(op)
#test for whiteness#
Box.test(fit.arima$residuals,lag=20,type="Box-Pierce")
Box.test(fit.arima$residuals,lag=20,type="Ljung-Box")

shapiro.test(fit.arima$residuals)
jarque.bera.test(fit.arima$residuals)

qqnorm(fit.arima$residuals); qqline(fit.arima$residuals)
#make prediction#

plot(forecast(fit.arima, h=12),ylab="centered logscaled ashort")

ts.plot(exp(ashort),exp(mean(ashort)+pred1$pred),col=1:2,main="Forcasting")
#real value#
pred1<-predict(fit.arima,n.ahead=12)
#transform back to the original form and calculate the CI#
result.pred<-data.frame(prediction=exp(pred1$pred+mean(ashort)),lower=exp(mean(ashort)+pred1$pred-1.96*pred1$se),upper=exp(mean(ashort)+pred1$pred+1.96*pred1$se))
result.pred

#compare the result#
airpass<-read.table("airpass.tsm")
pred2<-exp(pred1$pred+mean(ashort))
error<-as.vector(pred2)-as.vector(airpass[133:144,])
sum(error*error)/12
h=20
par(mfrow=c(3,2))
#a#
acf.AR2=ARMAacf(ar=c(-0.2,0.48),ma=,lag.max=h,pacf=FALSE)
plot(seq(0,h),acf.AR2,type="h",pch=22,lty=1,pty=2,xlab="Lag",ylab="ACF",main="(a) (1+0.2B-0.48B^2)Xt = Zt",ylim=c(-1.0,1.0))
abline(h=0)
pacf.AR2=ARMAacf(ar=c(-0.2,0.48),ma=,lag.max=h,pacf=TRUE)
plot(seq(0,h),rbind(c(1,pacf.AR2)),type="h",pch=22,lty=1,pty=2,xlab="Lag",ylab="PACF",main="(a) (1+0.2B-0.48B^2)Xt = Zt",ylim=c(-1.0,1.0))
abline(h=0)
#c#
acf.ARMR11=ARMAacf(ar=-0.6,ma=1.2,lag.max=h,pacf=FALSE)
plot(seq(0,h),acf.ARMR11,type="h",pch=22,lty=1,pty=2,xlab="Lag",ylab="ACF",main="(c) (1+0.6B)Xt = (1+1.2B)Zt",ylim=c(-1.0,1.0))
abline(h=0)
pacf.ARMR11=ARMAacf(ar=-0.6,ma=1.2,lag.max=h,pacf=TRUE)
plot(seq(0,h),rbind(c(1,pacf.ARMR11)),type="h",pch=22,lty=1,pty=2,xlab="Lag",ylab="PACF",main="(c) (1+0.6B)Xt = (1+1.2B)Zt",ylim=c(-1.0,1.0))
abline(h=0)
#d#
acf.AR3=ARMAacf(ar=c(-1.8,-.81),ma=,lag.max=h,pacf=FALSE)
plot(seq(0,h),acf.AR3,type="h",pch=22,lty=1,pty=2,xlab="Lag",ylab="ACF",main="(d) (1+1.8B+0.81B^2)Xt =Zt",ylim=c(-1.0,1.0))
abline(h=0)
pacf.AR3=ARMAacf(ar=c(-1.8,-.81),ma=,lag.max=h,pacf=TRUE)
plot(seq(0,h),rbind(c(1,pacf.AR3)),type="h",pch=22,lty=1,pty=2,xlab="Lag",ylab="PACF",main="(d) (1+1.8B+0.81B^2)Xt =Zt",ylim=c(-1.0,1.0))
abline(h=0)
#3.9#
deaths=read.table("deaths.txt",sep="")
deaths=as.matrix(deaths)
plot(deaths,type="o",pch=22,lty=1,pty=2,xlab="",ylab="(in thousand)",main="Accidental death")
abline(h=0)
deaths.dif<-diff(deaths,lag=12)
deaths.diff<-diff(deaths.dif)
plot(deaths.diff,type="o",pch=22,lty=1,pty=2,xlab="",ylab="(in thousand)",main="Accidental death")
abline(h=0)
mean(deaths.diff)
sample.acf<-acf(deaths.diff,lag.max=20,main="Sample ACF of accidental death")
sample.acf$acf[c(2,12,13)]
sample.acvf<-acvf(deaths.diff,h=20)
sample.acvf[1]
par(op)
#3.10#
strikes=read.table("strikes.txt",sep="")
strikes=as.matrix(strikes)
strikes.dif<-diff(strikes,lag=1)
mean(strikes)
sample.acf<-acf(strikes,lag.max=20,main="Sample ACF of accidental death")
sample.acf$acf[c(2)]
sample.acvf<-acvf(strikes,h=20)
sample.acvf[1]



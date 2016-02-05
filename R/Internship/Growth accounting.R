#1#
install.packages("xlsx")
library(xlsx)
library(rJava)
#read file#
dat<-read.xlsx(file.choose(),sheetName="Data",head=TRUE)
#initial data#
alpha1<-0.5
data.china<-data.frame(rgdpna.det=diff(dat$rgdpna)/dat$rgdpna[-length(dat$rgdpna)],rkna.det=diff(dat$rkna)/dat$rkna[-length(dat$rkna)],emp.det=diff(dat$emp)/dat$emp[-length(dat$emp)])
#compute residuals#
data.china<-cbind(data.china,data.frame(det.z=data.china$rgdpna.det-alpha1*data.china$rkna.det-(1-alpha1)*data.china$emp.det))
#plot data#
View(data.china)

#view data in time series form#
data.ts<-ts(data.china$det.z,start=c(1991),frequency=1)
plot(data.ts,xaxt="n",ylab="Solow residuals",main="Plot for Solow residuals")
axis(1,at=seq(1991,2010,1),cex=1)

#2#
#preparation#
install.packages("SDMTools")
library(SDMTools)
dat2<-read.xlsx(file.choose(),sheetName="Data1",head=TRUE)
dat2<-cbind(dat2,data.frame(gdp.percapita=log(dat2$rgdpna/dat2$pop)))
#initial value#
sd.unweight<-vector()
sd.weighted<-vector()
missing<-vector()
#using loop to compute the unweighted and weighted sd#
for (i in 1: 21){
 sd.unweight[i]<-sd(dat2[which(dat2$year==1990+i),5])
 sd.weighted[i]<-wt.sd(dat2[which(dat2$year==1990+i),5],dat2[which(dat2$year==1990+i),3]/sum(dat2[which(dat2$year==1990+i),3]))

}
#plot the result#
plot(ts(sd.unweight,start=c(1991)),xaxt="n",type="o",ylim=c(1,1.26),main="Weighted and unweighted sd for GDP per Capita",col="red",xlab="year",ylab="sd")
axis(1,at=seq(1991,2011,1),cex=1)
#ad legend#
legend("bottomleft",c("Unweighted sd","Weighted sd"), col=c("red","blue"),lty=c(1,1))
lines(ts(sd.weighted,start=c(1991)),type="o",col="blue")


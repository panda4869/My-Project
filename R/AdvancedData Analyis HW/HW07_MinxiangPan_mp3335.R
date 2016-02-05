##Problem 1
#(a)
library(MASS)
data(ChickWeight)
View(ChickWeight)
Chick.dat<-data.frame(weight=factor(1*(ChickWeight$weight>215)),Time=ChickWeight$Time,Chick=ChickWeight$Chick,Diet=ChickWeight$Diet)
#regression
glm.14<-glm(weight~Diet,data=Chick.dat[which(Chick.dat$Diet==1 & Chick.dat$Time==21 | Chick.dat$Diet==4 & Chick.dat$Time==21),],family='binomial')
summary(glm.14)
exp(glm.14$coefficients)
summary(stepAIC(glm.14,direction=c("both")))
anova(glm.14)
#(b)
row.mat<-match(ChickWeight[which(Chick.dat$Diet==1 & Chick.dat$Time==21 | Chick.dat$Diet==4 & Chick.dat$Time==21),]$Chick,ChickWeight[which(ChickWeight$Time==0),]$Chick)
Weight.b<-ChickWeight[which(ChickWeight$Time==0),]$weight[row.mat]
dat<-cbind(Chick.dat[which(Chick.dat$Diet==1 & Chick.dat$Time==21 | Chick.dat$Diet==4 & Chick.dat$Time==21),],Weight.b)


View(dat)
glm.14.adj<-glm(weight~Diet+Weight.b,data=dat,family='binomial')
summary(glm.14.adj)
exp(glm.14.adj$coefficients)
summary(stepAIC(glm.14.adj,direction=c("both")))
#Problem2
#(a)
#regression
glm.all<-glm(weight~Diet,data=Chick.dat[which( Chick.dat$Time==21),],family='binomial')
summary(glm.all)
exp(glm.all$coefficients)
summary(stepAIC(glm.all,direction=c("both")))
#(b)
row.mat<-match(ChickWeight[which(Chick.dat$Time==21),]$Chick,ChickWeight[which(ChickWeight$Time==0),]$Chick)
Weight.b<-ChickWeight[which(ChickWeight$Time==0),]$weight[row.mat]
dat.ad<-cbind(Chick.dat[which(Chick.dat$Time==21),],Weight.b)
glm.all.adj<-glm(weight~Diet+Weight.b,data=dat.ad,family='binomial')
summary(glm.all.adj)
exp(glm.all.adj$coefficients)
summary(stepAIC(glm.all.adj,direction=c("both")))
#Problem3
#(a)
library(glmnet)
View(Chick.dat[which(Chick.dat$Diet==1 & Chick.dat$Time==21 | Chick.dat$Diet==4 & Chick.dat$Time==21),])
x<-model.matrix(weight~Diet,data=Chick.dat[which(Chick.dat$Diet==1 & Chick.dat$Time==21 | Chick.dat$Diet==4 & Chick.dat$Time==21),])[,-1]
y<-as.factor(Chick.dat[which( Chick.dat$Diet==1 & Chick.dat$Time==21 | Chick.dat$Diet==4 & Chick.dat$Time==21),]$weight)
#cross validation
fit.cv<-cv.glmnet(x,y,alpha=1,family='binomial')
plot(fit.cv)
fit.best<-glmnet(x,y,alpha=1,family='binomial',lambda=fit.cv$lambda.min)
coef(fit.best)
exp(coef(fit.best))


#(b)
x1<-model.matrix(weight~Diet+Weight.b,data=dat)[,-1]
y1<-as.factor(dat$weight)
#cross validation
fit.ad.cv<-cv.glmnet(x1,y1,alpha=1,family='binomial')
plot(fit.ad.cv)
fit.ad.best<-glmnet(x1,y1,alpha=1,family='binomial',lambda=fit.ad.cv$lambda.min)
coef(fit.ad.best)
exp(coef(fit.ad.best))


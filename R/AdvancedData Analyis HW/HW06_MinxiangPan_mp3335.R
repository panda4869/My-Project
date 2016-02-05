#problem 1
data(ChickWeight)
View(ChickWeight)
class(ChickWeight$Diet)
#anova on original data
fit1<-lm(weight~Diet,data=ChickWeight[which(ChickWeight$Time==18),])
anova(fit1)
row.mat<-match(ChickWeight[which(ChickWeight$Time==18),]$Chick,ChickWeight[which(ChickWeight$Time==0),]$Chick)
#adjust for birthweight
#combine the data
Weight.b<-ChickWeight[which(ChickWeight$Time==0),]$weight[row.mat]
Diet.ad<-ChickWeight[which(ChickWeight$Time==18),]$Diet
dat<-cbind(ChickWeight[which(ChickWeight$Time==18),],Weight.b)
plot(weight~Diet,data=dat)
View(ChickWeight[which(ChickWeight$Time==18),])
View(ChickWeight[which(ChickWeight$Time==0),])
#add an additional varaible
fit2<-lm(weight~Weight.b+Diet,data=dat)
summary(fit2)
summary(aov(weight~Weight.b+Diet,data=dat))

##lsmean
install.packages("lsmeans")
library("lsmeans")
library("estimability")
fit1.rg1<-ref.grid(fit1)
lsmeans(fit1.rg1,"Diet")
fit2.rg1<-ref.grid(fit2)
lsmeans(fit2.rg1,"Diet")
tapply(dat$weight, dat$Diet, FUN=mean)
tapply(dat$Weight.b, dat$Diet, FUN=mean)
# normality
hist(resid(fit1))
qqnorm(resid(fit1))
qqline(resid(fit1))
shapiro.test(resid(fit1))
#it is normal
#
par(mfrow=c(1,2))
hist(resid(fit2))
qqnorm(resid(fit2))
qqline(resid(fit2))
shapiro.test(resid(fit2))

#unequal variance
bartlett.test(weight~Diet,data=ChickWeight[which(ChickWeight$Time==18),])
bartlett.test(weight-Weight.b~Diet,data=dat)
#variance are equal
#Test for Parallelism
summary(aov(weight~Weight.b*Diet,data=dat))
#no parallelism

#Problem 2
library(nlme)
#compound symmetry structure
fit.gls<-gls(weight~Diet*Time,data=ChickWeight[which(ChickWeight$Time==10 |ChickWeight$Time==18 | ChickWeight$Time==21),],
             correlation=corCompSymm(form=~1|Chick),method="REML")
summary(fit.gls)
anova(fit.gls)
#unstructured covariance
fit.ustr<-gls(weight~Diet*Time,data=ChickWeight[which(ChickWeight$Time==10 |ChickWeight$Time==18 | ChickWeight$Time==21),],
              correlation=corSymm(form=~1|Chick),weights=varIdent(form=~1|Time),method="REML")
summary(fit.ustr)
anova(fit.ustr)
anova(fit.gls,fit.ustr)

# normality test
hist(resid(fit.gls))
qqnorm(resid(fit.gls))
qqline(resid(fit.gls))
shapiro.test(resid(fit.gls))
hist(resid(fit.ustr))
qqnorm(resid(fit.ustr))
qqline(resid(fit.ustr))
shapiro.test(resid(fit.ustr))
# maybe we should try Mauchly's sphericity test
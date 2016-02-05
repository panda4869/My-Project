#Problem 1a
library(MASS)
data(crabs)
View(crabs)
#parametric procedure
#assmume independent normality
sample.B<-crabs[which(crabs$sp=="B"),"CL"]
sample.O<-crabs[which(crabs$sp=="O"),"CL"]
t.test(sample.B,sample.O,var.equal=TRUE)

#wilcox's rank test
wilcox.test(sample.B,sample.O)
#re-sampling procedure
#bootstrap
#method one
alpha<-rep(NA,1000)
set.seed(1)
l1<-length(sample.B)
l2<-length(sample.O)
for (i in 1:1000){
  group1<-sample(sample.B,l1,replace=TRUE)
  group2<-sample(sample.O,l2,replace=TRUE)
  alpha[i]<-t.test(group1,group2)$p.value  
}
#method two
n1<-length(sample.B)
n2<-length(sample.O)
z.o<-(mean(sample.B)-mean(sample.O))/sqrt(var(sample.B)/n1+var(sample.O)/n2)
z.b<-rep(NA,1000)
set.seed(1)
for (i in 1:1000){
  group1<-sample(sample.B,n1,replace=TRUE)
  group2<-sample(sample.O+mean(sample.B)-mean(sample.O),n2,replace=TRUE)
  z.b[i]<-(mean(group1)-mean(group2))/sqrt(var(group1)/n1+var(group2)/n2) 
}
length(which(abs(z.b)>=abs(z.o)))/1000

#chance of reject null hypothsis at the significant level of 95%
mean(alpha<0.05)
#Problem 1b
#paired t-test
t.test(sample.B,sample.O,paired=T,var.equal=TRUE)
#check for normality
par(mfrow=c(1,2))
hist(sample.B,main="Blue crab")
lines(density(sample.B))
hist(sample.O,main="Orange crab")
lines(density(sample.O))
qqnorm(sample.B)
qqline(sample.B)
qqnorm(sample.O)
qqline(sample.O)
shapiro.test(sample.B)
shapiro.test(sample.O)
#var test
var.test(sample.B,sample.O)
#Problem3
drinker.data <- array(c(38, 26, 52, 61, 65, 94,147, 153,30,56,42,102), c(2, 2, 3),dimnames=list(c("at least one drink","less than one drink"),c("cases","controls"),c("<21kg/m3","21~25kg/m3",">25kg/m3")))
mantelhaen.test(drinker.data)
#Problem4
hand.data<-matrix(c(149,129,48,68),2,2,byrow=FALSE)
#prop test
prop.test(c(149,129),c(197,197))
#fisher test
fisher.test(hand.data)


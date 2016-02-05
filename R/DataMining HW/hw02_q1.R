#code for q1(a)
p1<-read.csv("hw02_q1_p1_fall14.csv")
apply(p1,1,mean)
apply(p1,2,mean)
#code for q1(b)
a=scale(p1,center=T,scale=F)
cov(a)
#code for q1(c)
eigen(cov(a))
#code for q1(d)
pr.out=prcomp(p1,center=T,scale=T)
names(pr.out)
pr.out$rotation
pr.out$x
dim(pr.out$x)
#code for q1(e)
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
#code for q1(f)
pr.a=princomp(p1,center=T,scale=T,cor=T)
p2<-read.csv("hw02_q1_p2_fall14.csv")
pp=data.matrix(p2)
pp1=scale(pp,center=T,scale=T)
s=pp1%*%pr.a$loadings
s
#code for q1(g)
M=pr.a$loadings[,1:2]
pp2=(pp1%*%M)%*%t(M)
c=rbind(pp1,pp2)
dist1=dist(c,method="euclidean")
dist1=data.matrix(dist1)
a=dist1[6:10, 1:5]
diag(a)
#code for q1(h)
d=pp2-pp1
d
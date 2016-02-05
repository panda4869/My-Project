x=c(7,12,4,14,25,30)
y=c(128,213,75,250,446,540)
result<-lm(y~1+x)
summary(result)
plot(x,y);abline(result)

normal <- function(Bata){
  b<- Bata[1]
  
    logL <- -0.5*N*log(2*pi) - N*log(4) - sum(0.5*(y - b*x)^2/4^2)
return (logL)
 }
N=length(x)
result <- maxLik(normal,start=c(17))
print(result)


sum((y-predict(result,data=x))^2)/4*(1/4+(28-22.5)^2/207.5)
point<-data.frame(x=28)
lm.pews<-predict(result,point,interval="prediction",level=0.95)
lm.pews
> newPoint=data.frame(V2=28)
> s23=predict(lm(V1~1+V2),newPoint,interval="confidence", level =0.95)

anova(lm(V1~1+V2))
anova(result)

mydata=read.table("CH01PR19.txt",header=F)

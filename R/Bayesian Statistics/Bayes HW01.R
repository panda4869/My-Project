#4#
#Preparation#
#read data#
install.packages("foreign")
library(foreign)
research_center<-read.dta("pew_research_center_june_elect_wknd_data.dta")
Election_Result<-read.csv("2008ElectionResult.csv")
research_center=research_center[-which(is.na(research_center$ideo)| research_center$ideo=='missing/not asked'| research_center$ideo=='dk/refused'),]
#eliminate the HI and AL#
attach(Election_Result)
Election_Result_adjust=Election_Result[-which(state=="Hawaii" | state=="Alaska"),]
detach(Election_Result)

library(gsubfn)
library(proto)
library(RSQLite)
library(DBI)
library(sqldf)
library(tcltk)
#using SQL to join the two table#
newdata<-sqldf("select state, sum(case when ideo='very liberal' then 1 else 0 end) as Num_very_liberal, count(*) as Num_people,  round(cast(sum(case when ideo='very liberal' then 1 else 0 end) as double)/ count(*),2) proportion from research_center where state is not 'hawaii' and state is not 'alaska' or state is 'washington dc' group by state")

newdata<-sqldf("select a.*, round(cast(b.vote_Obama_pct as double),4)/100 as vote_Obama_pct from newdata AS a inner join Election_Result_adjust b on a.state=LOWER(b.state)")
newdata[which(newdata=='washington dc'),1]=c('dc')
#Resort the Abbreviation#
AB=c(state.abb[-which(state.abb=='HI' | state.abb=='AK')],'DC')
B=vector()
for(i in 8:(length(AB)-1)){
B[i]=AB[i]   
}
for(i in 8:(length(AB)-1)){
  AB[i+1]=B[i]
}
AB[8]="DC" 

newdata.full=cbind(newdata,data.frame(ABB=AB))

#plot the data#
attach(newdata.full)
plot(vote_Obama_pct,proportion,xaxt="n",xlab='vote_Obama_share', ylab='liberal_proportion',col='black',text(vote_Obama_pct,proportion,labels=AB,cex=0.8),cex=0.1)
axis(1,at=seq(0,1,0.01),cex=0.5)
title("Vote_Share vs Proportion of Liberity")
detach(newdata.full)
#b#
newdata2=cbind(newdata.full,data.frame(Posterior_Mean=round((newdata.full$Num_very_liberal+1)/(newdata.full$Num_people+2),4)))
attach(newdata2)
plot(vote_Obama_pct,Posterior_Mean,xaxt="n",xlab='vote_Obama_share', ylab='Posterior_Mean',col='black',text(vote_Obama_pct,Posterior_Mean,labels=AB,cex=0.8),cex=0.1)
axis(1,at=seq(0,1,0.01),cex=0.5)
title("Vote_Share vs Posterior_Mean")
detach(newdata2)
#c#
par(mfrow=c(2,2))
attach(newdata.full)
plot(vote_Obama_pct,proportion,xaxt="n",xlab='vote_Obama_share', ylab='liberal_proportion',col='black',text(vote_Obama_pct,proportion,labels=AB,cex=0.8),cex=0.1)
axis(1,at=seq(0,1,0.01),cex=0.5)
title("Vote_Share vs Proportion of Liberity")
detach(newdata.full)
attach(newdata2)
plot(vote_Obama_pct,Posterior_Mean,xaxt="n",xlab='vote_Obama_share', ylab='Posterior_Mean',col='black',text(vote_Obama_pct,Posterior_Mean,labels=AB,cex=0.8),cex=0.1)
axis(1,at=seq(0,1,0.01),cex=0.5)
title("Vote_Share vs Posterior_Mean")
detach(newdata2)
attach(newdata.full)
plot(Num_people,proportion,xlab='Respondents', ylab='liberal_proportion',col='black',text(Num_people,proportion,labels=AB,cex=0.8),cex=0.1)
title("Respondents vs Proportion of Liberity")
detach(newdata.full)
attach(newdata2)
plot(Num_people,Posterior_Mean,xlab='Respondents', ylab='Posterior_Mean',col='black',text(Num_people,Posterior_Mean,labels=AB,cex=0.8),cex=0.1)
title("Respondents vs Posterior_Mean")
detach(newdata2)


#5a#
prior<-function(theta,w1,w2,w3){
  a<-w1 #first break point#
  b<-(w1+w3)/2 #second break point#
  c<-w3 #third break point#
  d<-w2 #area ratio#
  f<-d/(a-c+1)
  e<-(1-d-0.5*f*(c-a))/(0.5*(c-a))#slope#
  z<-ifelse(theta<=a,f,ifelse(theta<=b,((e-f)/(b-a))*theta+e-((e-f)/(b-a))*b,ifelse(theta<=c,((e-f)/(b-c))*theta+e+((e-f)/(c-b))*b,ifelse(theta<=1,f,0))))#function under each condition#
  return(z)
  
}


a=seq(0,1,0.001)
#plot the prior distribution#
plot(a,prior(a,0.385,0.4,0.585),yaxt="n",xlab="theta",ylab="denstity",type="l")
axis(2,at=seq(0,5.5,0.5))
title("Witch' hat distribution")
#posterior distribution#
posterior<-function(theta,y,n,w1,w2,w3){
  
  dens<-((theta)^(y)*(1-theta)^(n-y))*prior(theta,w1,w2,w3)

    p.y<-sum(dens)/length(theta)
 
 
  return(dens/p.y)  
}
a=seq(0,1,0.001)
#plot the posterior distribution#
plot(a,posterior(a,437,980,0.385,0.4,0.585),xaxs="i",yaxs="i",xlab="theta",ylab="denstity",type="l")
title("Posterior density function")
#calculate mean and variance#
mean1<-sum(a*posterior(a,437,980,0.385,0.4,0.585))/length(a)
mean1
var1<-sum(a^2*posterior(a,437,980,0.385,0.4,0.585))/length(a)-sum(a*posterior(a,437,980,0.385,0.4,0.585)/length(a))^2
var1
#compute the posterior median#
c<-posterior(a,437,980,0.385,0.4,0.585)/length(a)

med<-function(c){
  a<-1
for(i in 1:length(c))
 {if(abs(sum(c[1:i])-0.5)<a)
  {a<-abs(sum(c[1:i])-0.5)
   b<-(i-1)/1000
}
}
return(b) 
}
med.value<-med(c)
med.value

#compute the confidence interval#
qtl<-function(per,c){
  a<-1
  for(i in 1:length(c))
  {  
  if(abs(sum(c[1:i])-per)<a)
  {a<-abs(sum(c[1:i])-per)
   b<-(i-1)/1000
  }
  }
  return(b) 
}

confi<-function(per,c){
  con<-vector()
  con[1]<-qtl(1-per,c)
  con[2]<-qtl(per,c)
  return(con)
}
CI<-confi(0.975,c)
CI
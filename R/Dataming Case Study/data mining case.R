#############
# prblem 1
#############
library(lattice)
library(grid)
library(DMwR)
require(car)
#read data
head(algae)
hist(algae$mxPH,prob=T, main="Histogram of Maximum pH", ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH)
boxplot(algae$oPO4)
rug(jitter(algae$oPO4),side=2)
abline(h=mean(algae$oPO4,na.rm=T),lty=2)
#find outier
plot(algae$NH4,xlab="")
abline(h=mean(algae$NH4,na.rm=T),lty=2)
abline(h=mean(algae$NH4,na.rm=T)+sd(algae$NH4,na.rm=T),lty=2)
abline(h=median(algae$NH4,na.rm=T),lty=2)
#find the outier by clicking the number
plot(algae$NH4,xlab="")
click<-identify(algae$NH4)
algae[click,]
algae[!is.na(algae$NH4) & algae$NH4>19000,]
# boxplots for different nominal variable 
library(lattice)
bwplot(size~ a1,data=algae,ylab="River Size", xlab="Algae01")
# bwplot with dashed lines
library(Hmisc)
require(survival)
require(Formula)
bwplot(size~a1, data=algae,panel=panel.bpplot,probs=seq(0.01,0.49,by=.01), datadensity=TRUE, ylab='River Size',xlab='Algal A1')
# conditional plots with continuous variable
minO2<-equal.count(na.omit(algae$mnO2),number=4,overlap=1/5)
stripplot(season~a3|algae$mnO2, data=algae[!is.na(algae),])
# count the missing rows
nrow(algae[!complete.cases(algae),])
#remove the incomplete observations
algae<-na.omit(algae)
apply(algae,1, function(x) sum(is.na(x))
# locate the column number of the observations that exceeded the threshold 
manyNAs(algae,.2)
algae[is.na(algae$Chla),"Chla"]<-median(algae$Chla,na.rm=T)
# remove the observations with many NAs fill the rest with median
data(algae)
algae<-algae[-manyNAs(algae),]
algae<-centralImputation(algae)
# show the correlation between the variables
symnum(cor(algae[,4:18],use="complete.obs"))
# using linear relationship to fill the data
data(algae)
algae<-algae[-manyNAs(algae),]
fillP04<-function(oP){
   if(is.na(oP))
   return(NA)
   else return(42.879+1.293*oP)
}
algae[is.na(algae$PO4),"PO4"]<-sapply(algae[is.na(algae$PO4),"oPO4"],fillP04)
#conditional histogram to find the linear relasionship in nominal variables
histogram(~mxPH | season, data=algae)
#change the order
algae$season<-factor(algae$season,levels=c("spring","summer","autumn","winter"))
histogram(~mxPH |size*speed, data=algae)
stripplot(size~mxPH |speed, data=algae,jitter=T)
#fill the unknown value depend on each row
# using knnImputation
data(algae)
algae<-algae[-manyNAs(algae),]
algae<-knnImputation(algae,k=10,meth="median")
########################
#Prediction method
########################
#Clean the data
data(algae)
algae<-algae[-manyNAs(algae),]
clean.algae<-knnImputation(algae,k=10)
#regress on a1
lm.a1<-lm(a1~.,data=clean.algae[,1:12])
summary(lm.a1)
#remove the season#
lm2.a1<-update(lm.a1,.~.-season)
summary(lm2.a1)
#show the difference between two models#
anova(lm.a1,lm2.a1)
#use backward method two select the variables#
final.lm<-step(lm.a1)
summary(final.lm)
#regression tree
require(rpart)
rt.a1<-rpart(a1~.,data=algae[,1:12])
prettyTree(rt.a1)
rt2.a1<-prune(rt.a1,cp=0.08)
#snip.rpart
first.tree<-rpart(a1~.,data=algae[,1:12])
snip.rpart(first.tree,c(4,7))
prettyTree(first.tree)
snip.rpart(first.tree)
lm.predictions.a1<-predict(final.lm,clean.algae)
rt.predictions.a1<-predict(rt2.a1,algae)
#compute MAE#
mean(abs(lm.predictions.a1-algae[,"a1"]))
mean(abs(rt.predictions.a1-algae[,"a1"]))
#or we can use MSE#
mean((lm.predictions.a1-algae[,"a1"])^2)
mean((rt.predictions.a1-algae[,"a1"])^2)
#NMSE#
mean((lm.predictions.a1-algae[,"a1"])^2)/mean((mean(algae[,"a1"])-algae[,"a1"])^2)
mean((rt.predictions.a1-algae[,"a1"])^2)/mean((mean(algae[,"a1"])-algae[,"a1"])^2)
#regr.eval compute all the things mentioned#
regr.eval(algae[,"a1"],lm.predictions.a1,train.y=algae[,"a1"])
par(mfrow=c(1,2))
plot(lm.predictions.a1,algae[,"a1"],main="linear model", xlab="Predictions",ylab="True value")
abline(0,1,lty=2)
plot(rt.predictions.a1,algae[,"a1"],main="linear model", xlab="Predictions",ylab="True value")
abline(0,1,lty=2)
#add some information#
sensible.lm.predictions<-ifelse(lm.predictions.a1<0,0,lm.predictions.a1)

regr.eval(algae[,"a1"],lm.predictions.a1,stat=c("mae","mse"))
#define cv function#
cv.rpart<-function(form,train,test,...){
  m<-rpartXse(form,train,...)
  p<-predict(m,test)
  mse<-mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2)) 
}
cv.lm<-function(form,train,test,...){
  m<-lm(form,train,...)
  p<-predict(m,test)
  p<-ifelse(p<0,0,p)
  mse<-mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))
}


#experimentalComparison#
res<-experimentalComparison(c(dataset(a1~.,clean.algae[,1:12],'a1')),c(variants('cv.lm'),variants('cv.rpart',se=c(0,0.5,1))),cvSettings(3,10,1234))
summary(res)
getVariant("cv.rpart.v1", res)
plot(res)

DS<-sapply(names(clean.algae)[12:18],
           function(x,name.attrs){
  f<-as.formula(paste(x,"~."))
  dataset(f,clean.algae[,c(name.attrs,x)],x)  
},
names(clean.algae)[1:11])

res.all<-experimentalComparison(DS,c(variants('cv.lm'),variants('cv.rpart',se=c(0,0.5,1))),cvSettings(5,10,1234))
plot(res.all)
#using randomforest#
library(randomForest)

cv.rf<-function(form,train,test,...){
  m<-randomForest(form, train,...)
  p<-predict(m,test)
  mse<-mean((p-resp(form,test))^2)
  c(nmse=mse/mean((mean(resp(form,train))-resp(form,test))^2))  
}
res.all<-experimentalComparison(DS,c(variants('cv.lm'),variants('cv.rpart',se=c(0,0.5,1)),variants('cv.rf',ntree=c(200,500,700))),cvSettings(5,10,1234))
bestScores(res.all)
#wilcoxon test#
compAnalysis(res.all,against='cv.rf.v3',datasets=c('a1','a2','a4','a6'))
#select models for seven algae at the same time#
bestModelsNames<-sapply(bestScores(res.all), function(x) x['nmse','system'])
learners<-c(rf='randomForest',rpart='rpartXse')
View(learners)
#get the abb for the model#
funcs<-learners[sapply(strsplit(bestModelsNames,'\\.'),function(x) x[2])]
View(funcs)
#get parameters for models#
bestModels<-list()
parSetts<-lapply(bestModelsNames, function(x) getVariant(x,res.all)@pars)
for(a in 1:7){
  form<-as.formula(paste(names(clean.algae)[11+a],'~.'))
  bestModels[[a]]<-do.call(funcs[a],c(list(form,clean.algae[,c(1:11,11+a)]),parSetts[[a]]))
  
  
}
clean.test.algae<-knnImputation(test.algae,k=10,distData=algae[,1:11])
preds<-matrix(ncol=7,nrow=140)
for(i in 1:nrow(clean.test.algae)){
  
  preds[i,]=sapply(1:7,function(x) predict(bestModels[[x]],clean.test.algae[i,]))
  
  
}

avg.preds<-apply(algae[,12:18],2,mean)
apply((algae.sols-preds)^2,2,mean)/apply(scale(algae.sols,avg.preds,F)^2,2,mean)

######################
#Case two#
######################
require(xts)
require(zoo)
x1<-xts(rnorm(100),seq(as.POSIXct("2000-01-01"),len=100,by="day"))
x1[1:5]
x2<-xts(rnorm(100),seq(as.POSIXct("2000-01-01 13:00"),len=100,by="min"))
x2[1:5]
x3<-xts(rnorm(3), as.Date(c('2005-01-01','2005-01-10','2005-01-12')))
x3
#from to#
x1['2000-03/']
x1['/2000-01-03']
mts.vals<-matrix(round(rnorm(25),2),5,5)
colnames(mts.vals)<-paste('ts',1:5,sep="")
mts<-xts(mts.vals,as.POSIXct(c('2003-01-01','2003-01-04','2003-01-05','2003-01-06','2003-02-16')))
mts
mts['2003-01',c("ts2","ts5")]
#extract the time tag#
index(mts)
#read in the data#
GSPC<-as.xts(read.zoo("sp500.csv",header=T))
#download the data from the web#
library(tseries)
GSPC<-as.xts(get.hist.quote("^GSPC",start="1970-01-02",end="2009-09-15",quote=c("Open","High","Low","Close","Volume","AdjClose")))
library(quantmod)
library(TTR)
getSymbols("^GSPC")
View(GSPC)
colnames(GSPC)<-c("Open","High","Low","Close","Volume","AdjClose")
#get data from different website#
setSymbolLookup(IBM=list(name='IBM',src='yahoo'),USDEUR=list(name='USD/EUR',src='oanda'))
getSymbols(c('IBM','USDEUR'))
T.ind<-function(quotes,tgt.margin=0.025,n.days=10){
  v<-apply(HLC(quotes),1,mean)
  r<-matrix(NA,nrow=NROW(quotes),ncol=n.days)
  for (x in 1:n.days) r[,x]<-Next(Delt(v,k=x),x)
  x<-apply(r,1,function(x) sum(x[x>tgt.margin|x< -tgt.margin]))
  if(is.xts(quotes))
    xts(x,time(quotes))
  else x
}

#candle Chart#
candleChart(last(GSPC,"3 months"),theme="white",TA=NULL)
avgPrice<-function(p) apply(HLC(p),1,mean)
addAvgPrice<-newTA(FUN=avgPrice,col=1,legend="AvgPrice")
addT.ind<-newTA(FUN=T.ind,col="red",legend="tgtRet")
addAvgPrice(on=1)
addT.ind()

#define indicators#
myATR<-function(x) ATR(HLC(x))[,"atr"]
mySMI<-function(x) SMI(HLC(x))[,"SMI"]
myADX<-function(x) ADX(HLC(x))[,"ADX"]
myAroon<-function(x) aroon(x[,c("High","Low")])$oscillator
myBB<-function(x) BBands(HLC(x))[,"pctB"]
myChaikinVol<-function(x) Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV<-function(x) EMA(CLV(HLC(x)))[,1]
myEMV<-function(x) EMV(x[,c("High","Low")],x[,"Volume"])[,2]
myMACD<-function(x) MACD(Cl(x))[,2]
myMFI<-function(x) MFI(x[,c("High","Low","Close")],x[,"Volume"])
mySAR<-function(x) SAR(x[,c("High","Close")])[,1]
myVolat<-function(x) volatility(OHLC(x), calc="garman")[,1]

#building our model#
data(GSPC)
library(randomForest)
data.model<-specifyModel(T.ind(GSPC)~Delt(Cl(GSPC),k=1:10)+myATR(GSPC)+mySMI(GSPC)+
                           myADX(GSPC)+myAroon(GSPC)+myBB(GSPC)+myChaikinVol(GSPC)+
                           myCLV(GSPC)+myEMV(GSPC)+myMACD(GSPC)+myMFI(GSPC)+mySAR(GSPC)+
                           myVolat(GSPC)+runMean(Cl(GSPC))+runSD(Cl(GSPC))+CMO(Cl(GSPC))+
                           EMA(Delt(Cl(GSPC)))+RSI(Cl(GSPC)))
set.seed(1234)
rf<-buildModel(data.model,method='randomForest',training.per=c(start(GSPC),index(GSPC["1999-12-31"])),
               ntree=50,importance=T)
#check the importance#
varImpPlot(rf@fitted.model,type=1) 
imp<-importance(rf@fitted.model,type=1)
#show the important features#
rownames(imp)[which(imp>10)]

data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC), k = 1) +
                           myATR(GSPC) + myADX(GSPC) + myEMV(GSPC) + myVolat(GSPC) +
                             myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)))

Tdata.train<-as.data.frame(modelData(data.model,data.window=c('1970-01-02','1991-12-31')))
Tdata.eval<-na.omit(as.data.frame(modelData(data.model,data.window=c('2000-01-01','2009-09-15'))))
Tform<-as.formula('T.ind.GSPC~.')

#ANN#
set.seed(1234)
library(nnet)
norm.data<-scale(Tdata.train)
nn<-nnet(Tform,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,linout=T,trace=F)
norm.preds<-predict(nn,norm.data[1001:2000,])
preds<-unscale(norm.preds,norm.data)
#transform to the signals#
sigs.nn<-trading.signals(preds,0.1,-0.1)
true.sigs<-trading.signals(Tdata.train[1001:2000,"T.ind.GSPC"],0.1,-0.1)
sigs.PR(sigs.nn,true.sigs)
#classification using ANN#
set.seed(1234)
library(nnet)
signals<-trading.signals(Tdata.train[,"T.ind.GSPC"],0.1,-0.1)
norm.data<-data.frame(signals=signals,scale(Tdata.train[,-1]))
nn<-nnet(signals~.,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,trace=F)
preds<-predict(nn,norm.data[1001:2000,],type="class")
sigs.PR(preds,norm.data[1001:2000,1])
#SVM#
library(e1071)
sv<-svm(Tform,Tdata.train[1:1000,],gamma=1/ncol(Tdata.train[1:1000,]),cost=100)
s.preds<-predict(sv,Tdata.train[1001:2000,])
sigs.svm<-trading.signals(s.preds,0.1,-0.1)
true.sigs<-strading.signals(Tdata.train[1001:2000,"T.ind.GSPC"],0.1,-0.1)
sigs.PR(sigs.svm,true.sigs)
#kernlab#
install.packages("kernlab")
library(kernlab)
data <- cbind(signals = signals, Tdata.train[, -1])
ksv <- ksvm(signals ~ ., data[1:1000, ], C = 10)
ks.preds <- predict(ksv, data[1001:2000, ])
sigs.PR(ks.preds, data[1001:2000, 1])

#Multivariate Adaptive Regression Splines#
library(earth)
e<-earth(Tform,Tdata.train[1:1000,])
e.preds<-predict(e,Tdata.train[1001:2000,])
sigs.e<-trading.signals(e.preds,0.1,-0.1)
true.sigs<-trading.signals(Tdata.train[1001:2000,"T.ind.GSPC"],0.1,-0.1)
sigs.PR(sigs.e,true.sigs)

#policy#
policy.1<-function(signals,market,opened.pos,money,bet=0.2,hold.time=10,exp.prof=0.025,max.loss=0.05)
{
  d<-NROW(market) # this is the ID of today
  orders<-NULL
  nOs<-NROW(opened.pos)
  # nothing to do!
  if(!nOs && signals[d]=="h") return(orders)
  #First lets check if we can open a new position
  #i) long positions
  if (signals[d]=='b'&& !nOs){
    quant<-round(bet*money/market[d,'Close'],0)
    if(quant>0)
      orders<-rbind(orders,data.frame(order=c(1,-1,-1)
                                      ,order.type=c(1,2,3),val=c(quant,
                                       market[d,'Close']*(1+exp.prof),
                                       market[d,'Close']*(1-max.loss)),
                                      action=c('open','close','close'),
                                      posID=c(NA,NA,NA)))
    
    
   #ii)#short positions 
  }else if(signals[d]=='s' && !nOs){
    #this is the nr of stocks we already need to buy
    #because of currently opened short positions
    need2buy<-sum(opened.pos[opened.pos[,'pos.type']==-1,"N.stocks"])*market[d,'Close']
    quant<-round(bet*(money-need2buy)/market[d,'Close'],0)
    if (quant>0)
      orders<-rbind(orders,data.frame(order=c(-1,1,1),order.type=c(1,2,3),
                                      val=c(quant,market[d,'Close']*(1-exp.prof),
                                            market[d,'Close']*(1+max.loss)),
                                      action=c('open','close','close'),
                                      posID=c(NA,NA,NA)))
      
  }
  # Now lets check if we need to close positions
  #because their holding time is over
  if(nOs)
    for(i in 1: nOs){
      if (d-opened.pos[i,'Odate']>=hold.time)
        orders<-rbind(orders,data.frame(order=-opened.pos[i,'pos.type'],order.type=1
                                        ,val=NA,action='close',posID=rownames(opened.pos)[i]))
      
    }
  
  
  
  orders
  
}


#predictions
#Tran and test periods
start<-1
len.tr<-1000
len.ts<-500
tr<-start:(start+len.tr-1)
ts<-(start+len.tr):(start+len.tr+len.ts-1)
#getting the quotes for the testing period
data(GSPC)
date<-rownames(Tdata.train[start+len.tr,])
market<-GSPC[paste(date,'/',sep='')][1:len.ts]#after the training period
#learning the model and obtaining its signal predictions
library(e1071)
s<-svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p<-predict(s,Tdata.train[ts,])
sig<-trading.signals(p,0.1,-0.1)
#now using the simulated trader
t1<-trading.simulator(market,sig,'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))

summary(t1)
tradingEvaluation(t1)
plot(t1,market,theme='white',name='SP500')
#experiment comparison#
MC.svmR<-function(form,train,test,b.t=0,1,s.t=-0.1,...){
  require(e1071)
  t<-svm(form,train,...)
  p<-predict(t,test)
  trading.signals(p,b.t,s.t)
}
MC.svmC<-function(form,train,test,b.t=0.1,s.t=-0.1...){
  require(e1071)
  tgtName<-all.vars(form)[1]
  train[,tgtName]<-trading.signals(train[,tgtName],b.t,s.t)
  t<-svm(form,train,...)
  p<-predict(t,test)
  factor(p,levels=c("s","h","b"))
  
}

MC.nnetR<-function(form,train,test,b.t=0.1.s.t=-0.1,...){
  require(nnet)
  t<-nnet(form,train,...)
  p<-predit(t,test)
  trading.signals(p,b.t,s.t)
    
}

MC.nnetC<-function(form,train,test,b.t=0.1,s.t=-0.1...){
  require(nnet)
  tgtName<-all.vars(form)[1]
  train[,tgtName]<-trading.signals(train[,tgtName],b.t,s.t)
  t<-nnet(form,train,...)
  p<-predit(t,test,type="class")
  factor(p,levels=c("s","h","b"))

}

MC.earth<-function(form,train,test,b.t=0.1.s.t=-0.1,...){
  require(earth)
  t<-earth(form,train,...)
  p<-predict(t,test)
  trading.signals(p,b.t,s.t)  
  
}

single<-function(form,train,test,learner,policy.func,...){
  p<-do.call(paste("MC",learner,sep="."),list(form,train,test,...))
  eval.stats(form,train,test,p,policy.func=policy.func)
}

slide<-function(form,train, test, learner, relearn.step,policy.func,...){
  real.learner<-learner(paste("MC",learner,sep="."),pars=list(...))
  p<-slidingWindowTest(real.learner,form,train,test,relearn.step)
  p<-factor(p,levels=1:3,labels=c("s","b","h"))
  eval.stats(form,train,test,p,policy.func=policy.func)
  
}

grow <- function(form, train, test, learner, relearn.step,
policy.func, ...) {
  real.learner <- learner(paste("MC", learner, sep = "."),
                          pars = list(...))
  p <- growingWindowTest(real.learner, form, train, test,
                         relearn.step)
  p <- factor(p, levels = 1:3, labels = c("s", "h", "b"))
  eval.stats(form, train, test, p, policy.func = policy.func)

}
eval.stats<-function(form,train,test,preds,b.t=0.1,s.t=-0.1,...){
  #signals evaluation
  tgtName<-all.vars(form)[1]
  test[,tgtName]<-trading.signals(test[,tgtName],b.t=0.1,s.t=-0.1)
  st<-sig.PR(preds,test[,tgtName])
  names(st)<-paste(rep(c('prec','rec'),each=3),c('s','b','sb'),sep='.')
  #Trading evaluatiion
  date<-rownames(test)[1]
  market<-GSPC[paste(date,'/',sep='')][1:length(preds),]
  trade.res<-trading.simulator(market,preds,...)
  c(st,tradingEvaluation(trade.res))
}

pol1<-function(signals,market,op,money){
  policy.1(signals,market,op,money,bet=0.2.exp.prof=0.025,max.loss=0.05,hold.time=20)
}

pol2<-function(signals,market,op,money){
  policy.2(signals,market,op,money,bet=0.5,exp.prof=0.05,max.loss=0.05)
}

#The list of learners we will use
TODO<-c('svmR','svmC','earth','nnetR','nnetC')
#The datasets used in the comparison
DSs<-list(dataset(Tform,Tdata.train,'SP500'))
#MC settings
MCsetts<-mcSettings(20,#20 repetitions of the MC exps
                    2540, # 10 years for training
                    1270,#5 years for testing
                    1234 # random number generator seed
                    )

# Variants to try for all learners
VARS<-list()
VARS$svmR<-list(cost=c(10,150),gamma=c(0.01,0.001),policy.function=c('pol1','pol2','pol3'))
VARS$svmC<-list(cost=c(10,150),gamma=c(0.01,0.001),policy.function=c('pol1','pol2','pol3')
VARS$earth<-list(nk=c(10,17),degree=c(1,2),thresh=c(0.01,0.001),policy.function=c('pol1','pol2','pol3')) 
VARS$nnetR<-list(linout=T,maxit=750,size=c(5,10),decay=c(0.001,0.01),policy.function=c('pol1','pol2','pol3')) 
VARS$nnetC<-list(maxit=750,size=c(5,10),decay=c(0.001,0.01),policy.function=c('pol1','pol2','pol3'))
#Main loop
for(td in TODO){
  assign(td, experimentalComparison(DSs,c(do.call('variants',c(list('single',learner=td),VARS[[td]],varsRootName=paste('single',td,sep='.'))),
                                          do.call('variants',c(list('slide',learner=td,re;earm/step=c(60.120)),VARS[[td]],
                                                               VarsRootName=paste('slide',td,sep='.'))),
                                          do.call('variants',c(list('grow',learner=td,relearn.step=c(60,120)),
                                                               VARS[[td]],
                                                               VarsRootName=paste('single',td,sep='.')))),
                                    MCsetts))
  
  
  save(list=td,file=paste(td,'Rdata',sep='.'))
  
}


#result analysis
load("svmR.Rdata")
load("svmC.Rdata")
load("earth.Rdata")
load("nnetR.Rdata")
load("nnetC.Rdata")

tgtStats<-c('prec.sb','Ret','PercProf'.'MaxDD','SharpeRatio')
allSysRes<-join(subset(svmR,stats=tgtStats),
                subset(svmC,stats=tgtStats),
                subset(nnetR,stats=tgtStats),
                subset(nnetC,stats=tgtStats),
                subset(earth,stats=tgtStats),by='variants')

rankSystems(allSysRes,5,max=c(T,T,T,F,T))


summary(subset(svmC,
                stats=c('Ret','RetOverBH','PercProf','NTrades'),
                vars=c('slide.svmC.v5','slide.svmC.v6')))
fullResults <- join(svmR, svmC, earth, nnetC, nnetR, by = "variants")
 nt <- statScores(fullResults, "NTrades")[[1]]
 rt <- statScores(fullResults, "Ret")[[1]]
 pp <- statScores(fullResults, "PercProf")[[1]]
 s1 <- names(nt)[which(nt > 20)]
 s2 <- names(rt)[which(rt > 0.5)]
s3 <- names(pp)[which(pp > 40)]
namesBest <- intersect(intersect(s1, s2), s3)
plot(subset(fullResults,
             stats=c('Ret','PercProf','MaxDD'), vars=namesBest))


#chapter 4#
library(DMwR)
data(sales)
library(ROCR)
library(gplots)
data(iris)
library(RWeka)
install.packages("RWeka")
WOW(AdaBoostM1)
data(iris)
library(DMwR)
library(e1071)
#load data
head(sales)
summary(sales)
nlevels(sales$ID)
nlevels(sales$Prod)
length(which(is.na(sales$Quant)& is.na(sales$Val)))
table(sales$Insp)/nrow(sales)*100
#number of reports
totS<-table(sales$ID)
totP<-table(sales$Prod)
barplot(totS,main="Transactions per salespeople", names.arg="",xlab="salespeople",ylab="Amount")
barplot(totP,main="Transations per product",names.arg="",xlab="Products",ylab="Amount")

sales$Uprice<-sales$Val/sales$Quant
summary(sales$Uprice)
#use the median unite price
attach(sales)
upp<-aggregate(Uprice,list(Prod),median,na.rm=T)
topP<-sapply(c(T,F),function(o)upp[order(upp[,2],decreasing=o)[1:5],1])
colnames(topP)<-c('Expensive','Cheap')
tops<-sales[Prod %in% topP[1,],c('Prod','Uprice')]
tops$Prod<-factor(tops$Prod)
boxplot(Uprice~Prod,data=tops,ylab="Uprice",log="y")
#salsepeople#
vs<-aggregate(Val,list(ID),sum,na.rm=T)
scoresSs<-sapply(c(T,F),function(o) vs[order(vs[,2],decreasing=o)[1:5],1])
colnames(scoresSs)<-c("Most","Least")
sum(vs[order(vs[,2],decreasing=T)[1:100],2])/sum(Val,na.rm=T)*100
sum(vs[order(vs[,2],decreasing=F)[1:2000],2])/sum(Val,na.rm=T)*100
#quantities
qs<-aggregate(Quant,list(Prod),sum,na.rm=T)
scoresPs<-sapply(c(T,F),function(o) qs[order(qs[,2],decreasing=o)[1:5],1])
colnames(scoresPs)<-c("Most","Least")
sum(as.double(qs[order(qs[,2],decreasing=T)[1:100],2]))/sum(as.double(Quant),na.rm=T)*100
sum(as.double(qs[order(qs[,2],decreasing=F)[1:4000],2]))/sum(as.double(Quant),na.rm=T)*100
#finding the outier#
out<-tapply(Uprice,list(Prod=Prod),function(x) length(boxplot.stats(x)$out))
out[order(out,decreasing=T)[1:10]]
sum(out)
totS<-table(ID)
totP<-table(Prod)
sum(out)/nrow(sales)*100
nas<-sales[which(is.na(Quant) & is.na(Val)),c("ID","Prod")]
propS<-100*table(nas$ID)/totS
propS[order(propS,decreasing=T)[1:10]]
propP<-100*table(nas$Prod)/totP
propP[order(propP,decreasing=T)[1:10]]
detach(sales)
sales<-sales[-which(is.na(sales$Quant) & is.na(sales$Val)),]
nnasQp<-tapply(sales$Quant,list(sales$Prod), function(x) sum(is.na(x)))
propNAsQp<-nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing=T)[1:10]]
sales<-sales[!sales$Prod %in% c("p2442","p2443"),]
nlevels(sales$Prod)
sales$Prod<-factor(sales$Prod)
nlevels(sales$Prod)
nnasVp<-tapply(sales$Val,list(sales$Prod),function(x) sum(is.na(x)))
propNAsVp<-nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp,decreasing=T)[1:10]]
nnasVs<-tapply(sales$Val,list(sales$ID),function(x) sum(is.na(x)))
propNAsVs<-nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs,decreasing=T)[1:10]]
#get all the unite price by product 
tPrice<-tapply(sales[sales$Insp!="fraud","Uprice"],list(sales[sales$Insp!="fraud","Prod"]),median,na.rm=T)
noQuant<-which(is.na(sales$Quant))
sales[noQuant,"Quant"]<-ceiling(sales[noQuant,"Val"]/tPrice[sales[noQuant,"Prod"]])
noVal<-which(is.na(sales$Val))
sales[noVal,"Val"]<-sales[noVal,"Quant"]*tPrice[sales[noVal,"Prod"]]
sales$Uprice<-sales$Val/sales$Quant
save(sales,file="salesClean.Rdata")

#obtain statistics
attach(sales)
notF<-which(Insp!="fraud")
ms<-tapply(Uprice[notF],list(Prod=Prod[notF]), function(x){
  bp<-boxplot.stats(x)$stats
  c(median=bp[3],iqr=bp[4]-bp[2])  
})
ms<-matrix(unlist(ms),length(ms),2,byrow=T,dimnames=list(names(ms),c("median","IQR")))
head(ms)
par(mfrow=c(1,2))
plot(ms[,1],ms[,2],xlab="Median",ylab="IQR",main="")
plot(ms[,1],ms[,2],xlab="Median",ylab="IQR",main="",col="grey",log="xy")
smalls<-which(table(Prod)<20)
points(log(ms[smalls,1]),log(ms[smalls,2]),pch="+")
#Kolmogorov-Smirnov test
dms<-scale(ms)
smalls<-which(table(Prod)<20)
prods<-tapply(sales$Uprice,sales$Prod,list)
head(prods)
similar<-matrix(NA,length(smalls),7,dimnames=list(names(smalls),c("Simil","ks.stat","ks.p","medP","iqrP","medS","iqrS")))
for(i in seq(along=smalls)){
  d<-scale(dms,dms[smalls[i],],FALSE)
  d<-sqrt(drop(d^2%*%rep(1,ncol(d))))
  stat<-ks.test(prods[[smalls[i]]],prods[[order(d)[2]]])
  similar[i,]<-c(order(d)[2],stat$statistic,stat$p.value,ms[smalls[i],],ms[order(d)[2],])
  
}

head(similar)
head(d)
#get the similar products#
levels(Prod)[similar[1,1]]
#confidence more than 0.9#
nrow(similar[similar[,"ks.p"]>=0.9,])
save(similar, file = "similarProducts.Rdata")
#PR curve
library(ROCR)
data(ROCR.simple)
pred<-prediction(ROCR.simple$predictions,ROCR.simple$labels)
perf<-performance(pred,"prec","rec")
plot(perf)

# interploted precision
RPcurve<-function(preds,trues,...){
  require(ROCR,quietly=T)
  pd<-prediction(preds,trues)
  pf<-performance(preds,"prec"."rec")
  pf@y.values<-lapply(pf@y.values,function(x) rev(cummax(rev(X))))
  plot(pf,...)  
}

PRcurve(ROCR.simple$predictions,ROCR.simple$labels)
perf<-performance(pred,"lift","rpp")
plot(perf,main="Lift Charts")
par(mfrow=c(1,1))
#cummulative recall chart
CRchart(ROCR.simple$predictions, ROCR.simple$labels, main='Cumulative Recall Chart')

#avgNDTP

avgNDTP<-function(toInsp,train,stats){
  if(missing(train)&& missing(stats))
    stop("Provide either training data or product stats")
  if(missing(stats)){
    notF<-which(train$Insp!="fraud")
    stats<-tapply(train$Uprice[notF],list(Prod=train$Prod[notF]),function(x) {
      bp<-boxplot.stats(x)$stats
      c(median=bp[3],iqr=bp[4]-bp[2])
        })
    stats<-matrix(unlist(stats),length(stats),2,byrow=T,dimnames=list(names(stats),c("median","iqr")))
    stats[which(stats[,"iqr"==0]),"iqr"]<-stats[which(stats[,'iqr']),"median"]  
  }
mdtp<-mean(abs(toInsp$Uprice-stats[toInsp$Prod,"median"])/stats[toInsp$Prod,"iqr"])
return(mdtp)
}
#evaluation
evalOutlierRanking<-function(testSet,rankOrder,Threshold,statsProds){
  ordTS<-testSet[rankOrder,]
  N<-nrow(testSet)
  nf<-if(Threshold<1) as.integer(N*Threshold) else Threshold
  cm<-table(c(rep("fraud",nf),rep("ok",N-nf)),ordTS$Insp)
  prec<-cm["fraud","fraud"]/sum(cm["fraud",])
  rec<-cm["fraud","fraud"]/sum(cm[,"fraud"])
  AVGGndtp<-avgNDTP(ordTS[nf,],stats=statsProds)
  return(c(Precision=prec,Recall=rec,avgNDTP=AVGGndtp))
  
}

# unsupervised approaches The modified box plot rule
BPrule<-function(train,test){
  notF<-which(train$Insp!="fraud")
  ms<-tapply(train$Uprice[notF],list(Prod=train$Prod[notF]),function(x){
    bp<-boxplot.stats(x)$stats
    c(median=bp[3],iqr=bp[4]-bp[2])    
  })
  ms<-matrix(unlist(ms),length(ms),2,byrow=T,dimnames=list(names(ms),c("median","iqr")))
  ms[which(ms[,"iqr"]==0),"iqr"]<- ms[which(ms[,"iqr"]==0),"median"]
  ORscore<-abs(test$Uprice-ms[test$Prod,"median"])/ms[test$Prod,"iqr"]
  return(list(rankOrder=order(ORscore,decreasing=T),rankScore=ORscore))  
}
#globalStats
 notF <- which(sales$Insp != 'fraud')
globalStats <- tapply(sales$Uprice[notF],list(Prod=sales$Prod[notF]),function(x) {
                        bp <- boxplot.stats(x)$stats
                        c(median=bp[3],iqr=bp[4]-bp[2]) 
                        })
globalStats <- matrix(unlist(globalStats),length(globalStats),2,byrow=T,dimnames=list(names(globalStats),c('median','iqr'))) 
globalStats[which(globalStats[,'iqr']==0),'iqr'] <-globalStats[which(globalStats[,'iqr']==0),'median']
head(globalStats)

ho.BPrule<-function(form,train,test,...){
  res<-BPrule(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=="fraud",1,0)))
  
  
}
bp.res<-holdOut(learner('ho.BPrule',pars=list(Threshold=0.1,statsProds=globalStats)),dataset(Insp~.,sales),hldSettings(3,0.3,1234,T),itsInfo=TRUE)
summary(bp.res)

par(mfrow=c(1,2))

info<-attr(bp.res,"itsInfo")
PTs.bp<-aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),c(1,3,2))

PRcurve(PTs.bp[,,1],PTs.bp[,,2],main="PR curve",avg="vertical")
CRchart(PTs.bp[,,1],PTs.bp[,,2],main="Cumulative Recall curve",avg="vertical")

#LOF#
ho.LOF<-function(form,train,test,k,...){
  ntr<-nrow(train)
  all<-rbind(train,test)
  N<-nrow(all)
  ups<-split(all$Uprice,all$Prod)
  r<-list(length=ups)
  for(u in seq(along=ups)){
    r[[u]]<-if(NROW(ups[[u]])>3) lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2))
    else if (NROW(ups[[u]]))  rep(0,NROW(ups[[u]]))
    else NULL
    all$lof<-vector(length=N)
    split(all$lof,all$Prod)<-r
    all$lof[which(!(is.infinite(all$lof) | !is.nan(all$lof)))]<-SoftMax(all$lof[which(!(is.infinite(all$lof) | !is.nan(all$lof)))])
 structure(evalOutlierRanking(test,order(all[(ntr+1):N,"lof"],decreasing=T),...),
           itInfo=list(preds=all[(ntr+1):N,"lof"],tures=ifelse(test$Insp=="fraud",1,0)))
  }
  
  
}

# hold out process
lof.res<-holdOut(learner('ho.LOF',pars=list(k=7,Threshold=0.1,statsProds=globalStats)),dataset(Insp~.,sales),
                 hldSettings(3,0.3,1234,T),itsInfo=TRUE)

summary(lof.res)


#supervised learning#
data(iris)
data<-iris[,c(1,2,5)]
data$Species<-factor(ifelse(data$Species=="setosa","rare","common"))
newData<-SMOTE(Species~.,data,perc.over=600)
table(newData$Species)

par(mfrow=c(1,2))
plot(data[,1],data[,2],pch=19+as.integer(data[,3]),main="Original Data")
plot(newData[,1],newData[,2],pch=19+as.integer(newData[,3]),main="SMOTE Data")
#naive bayes#
nb<-function(train,test){
  require(e1071,quietly=T)
  sup<-which(train$Insp!="Unkn")
  data<-train[sup,c("ID","Prod","Uprice","Insp")]
  data$Insp<-factor(data$Insp,levels=c("ok","fraud"))
  model<-naiveBayes(Insp~.,data)
  preds<-predict(model,test[,c("ID","Prod","Uprice","Insp")],type="raw")
  return(list(rankOrder=order(preds[,"fraud"],decreasing=T),rankScore=preds[,"fraud"]))
  
  
}
#hold out
ho.nb<-function(form,train,test,...){
  res<-nb(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=="fraud",1,0)))
  
}


nb.res<-holdOut(learner("ho.nb",pars=list(Threshold=0.1,statsProd=globalStats)),dataset(Insp~.,sales),hldSettings(3,0.3,1234,T),itsInfo=TRUE)
summary(nb.res)

#using smote data#

nb.s<-function(train,test){
  require(e1071,quietly=T)
  sup<-which(train$Insp!="unkn")
  data<-train[sup,c("ID","Prod","Uprice","Insp")]
  data$Insp<-factor(data$Insp,levels=c("ok","fraud"))
  newData<-SMOTE(Insp~.,data,perc.over=700)
  model<-naiveBayes(Insp~.,newData)
  preds<-predict(model,test[,c("ID","Prod","Uprice","Insp")],type="raw")
  return(list(rankOrder=order(preds[,"fraud"],decreasing=T),rankScore=preds[,"fraud"]))
}
ho.nbs<-function(form,train,test,...){
  res<-nb.s(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=="fraud",1,0)))
  
  
  
}

nbs.res<-holdOut(learner('ho.nbs',pars=list(Threshold=0.1,statsProd=globalStats)),dataset(Insp~.,sales),hldSettings(3,0.3,1234,T),itsInfo=TRUE)


summary(nbs.res)
par(mfrow=c(1,2))
info<-attr(nb.res,"itsInfo")
PTs.nb<-aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),c(1,3,2))
info<-attr(nbs.res,"itsInfo")
PTs.nbs<-aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),c(1,3,2))
PRcurve(PTs.nb[,,1],PTs.nb[,,2],main="PR curve",lty=1,xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(PTs.nbs[,,1],PTs.nbs[,,2],main="PR curve",lty=2,avg="vertical",add=T)
legend("topright",c("Naive Bayes","somteNaiveBayes"),lty=c(1,2),col=c("black","grey"),cex=0.7)
CRchart(PTs.nb[,,1],PTs.nb[,,2],main="Cumulative Recall curve",lty=1,xlim=c(0,1),ylim=c(0,1),avg="vertical")
CRchart(PTs.nbs[,,1],PTs.nbs[,,2],main="Cumulative Recall curve",lty=2,add=T,avg="vertical")
legend("bottomright",c("Naive Bayes","somteNaiveBayes"),lty=c(1,2),col=c("black","grey"),cex=0.7)

install.packages("RWeka")
library(RWeka)
WOW(AdaBoostM1)

data(iris)
idx<-sample(150,100)
model<-AdaBoostM1(Species~.,iris[idx,],control=Weka_control(I=100))
preds<-predict(model,iris[-idx,])
head(preds)
table(preds,iris[-idx,"Species"])


ab<-function(train,test){
  require(RWeka,quietly=T)
  sup<-which(train$Insp!="unkn")
  data<-train[sup,c("ID","Prod","Uprice","Insp")]
  data$Insp<-factor(data$Insp,levels=c("ok","fraud"))
  model<-AdaBoostM1(Insp~.,data,control=Weka_control(I=100))
  preds<-predict(model,test[,c("ID","Prod","Uprice","Insp")],type="probability")
  return(list(rankOrder=order(preds[,"fraud"]),decreasing=T,rankScore=preds[,"fraud"]))
  
  
  
}

ho.ab<-function(form,train,test,...){
  res<-ab(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=="fraud",1,0)))
  
}

ab.res<-holdOut(learner("ho.ab",pars=list(Threshold=0.1,statsProd=globalStats)),dataset(Insp~.,sales),
                hldSettings(3,0.3,1234,T),itsInfo=TRUE)


summary(ab.res)



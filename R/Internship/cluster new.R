    #################
# Setup
#################
require(NLP)
require(tm)
require(SnowballC)
install.packages("wordcloud")
require(wordcloud)
require(RColorBrewer)
require(cluster)
require(xlsx)
require(xlsxjars)
require(rJava)
install.packages("vegetarian")
require(vegetarian)
#################
# Problem 1a
#################

##########################################
# This code uses tm to preprocess the papers into a format useful for NB
preprocess.directory = function(dirname){
  
  # the directory must have all the relevant text files
  ds = DirSource(dirname)
  # Corpus will make a tm document corpus from this directory
  fp = Corpus( ds )
  # make all words lower case
  fp = tm_map( fp , content_transformer(tolower));
  # remove number
  fp = tm_map(fp, removeNumbers);
  # remove all punctuation
  fp = tm_map( fp, removePunctuation);
  # remove stopwords like the, a, and so on. 
  fp = tm_map( fp, removeWords, stopwords("english"));
  # remove stems like suffixes
  fp = tm_map( fp, stemDocument);
  # remove extra whitespace
  fp = tm_map( fp, stripWhitespace);
  return(fp)
}
data.corpus<-preprocess.directory("S&P500")
# create the matrix
data.dtm<-DocumentTermMatrix(data.corpus)
##################################
#method one: using weighted matrix#
##################################
#weighted dtm#
data.dtm.tfidf<-as.matrix(data.dtm)/apply(as.matrix(data.dtm),1,sum)
distmatrix<-dist(data.dtm.tfidf)
dtmfit<-hclust(distmatrix,method="ward.D")
#plot the result and save the result#
plot(dtmfit,cex=0.2,hang=-1,main="Dendrogram",xaxt="n",xlab="Company")
rect.hclust(dtmfit,k=10)
dtmfit.group<-as.matrix(cutree(dtmfit,k=10))
#re-order
dtmfit.group<-dtmfit.group[order(dtmfit.group,dtmfit.group[,1]),]
#save it in xlsx form#
write.xlsx(dtmfit.group,"cluster result.xlsx")
##############################################
#method two: using matrix withour sparse data#
##############################################
#eliminate the sparse data#
distmatrix<-dist(scale(as.matrix(removeSparseTerms(data.dtm,0.7))))
dtmfit<-hclust(distmatrix,method="ward.D")
#plot the result and save the result#
dev.copy(png,'myplot.png')
plot(dtmfit,cex=0.2,hang=-1,main="Dendrogram",xaxt="n",xlab="Company")
#cut the branch#
rect.hclust(dtmfit,k=10)
dev.off()
#show the result
dtmfit.group<-as.matrix(cutree(dtmfit,k=10))
#re-order
dtmfit.group<-dtmfit.group[order(dtmfit.group,dtmfit.group[,1]),]
View(dtmfit.group)

#save it in xlsx form#
write.xlsx(dtmfit.group,"cluster result.xlsx")

##############################################
#Words analysis#
##############################################
#findFreqTerms
findFreqTerms(data.dtm, lowfreq=1000)
#plot freq#
freq <- sort(colSums(as.matrix(data.dtm)), decreasing=TRUE)
head(freq, 14)
#wordcloud#
set.seed(123)
wordcloud(names(freq),freq,min.freq=5000)

#agn#
#agn<-agnes(dtm, diss = inherits(dtm, "dist"), metric = "euclidean", stand = FALSE, method = "average")
#plot(agn)
#hclust#
#get the first group#
group1.name<-names(dtmfit.group[which(dtmfit.group==1),])
data.dtm.group1<-as.matrix(data.dtm[group1.name,])
findFreqTerms(data.dtm[group1.name,], lowfreq=1000)
freq <- sort(colSums(data.dtm.group1), decreasing=TRUE)
head(freq, 20)
head(freq[round(0.05*length(freq)):length(freq)], 10)

head(freq[round(0.01*length(freq)):length(freq)], 10)

head(freq[!names(freq) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)

#get the second group#
group2.name<-names(dtmfit.group[which(dtmfit.group==2),])
data.dtm.group2<-as.matrix(data.dtm[group2.name,])
freq2<- sort(colSums(data.dtm.group2), decreasing=TRUE)
head(freq2[!names(freq2) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
#get the third group#
group3.name<-names(dtmfit.group[which(dtmfit.group==3),])
data.dtm.group3<-as.matrix(data.dtm[group3.name,])
freq3<- sort(colSums(data.dtm.group3), decreasing=TRUE)
head(freq3[!names(freq3) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
#get the forth group#
group4.name<-names(dtmfit.group[which(dtmfit.group==4),])
data.dtm.group4<-as.matrix(data.dtm[group4.name,])
freq4<- sort(colSums(data.dtm.group4), decreasing=TRUE)
head(freq4[!names(freq4) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
#get the fifth group#
group5.name<-names(dtmfit.group[which(dtmfit.group==5),])
data.dtm.group5<-as.matrix(data.dtm[group5.name,])
freq5<- sort(colSums(data.dtm.group5), decreasing=TRUE)
head(freq5[!names(freq5) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
#get the sixth group#
group6.name<-names(dtmfit.group[which(dtmfit.group==6),])
data.dtm.group6<-as.matrix(data.dtm[group6.name,])
freq6<- sort(colSums(data.dtm.group6), decreasing=TRUE)
head(freq6[!names(freq6) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
#get the seven group#
group7.name<-names(dtmfit.group[which(dtmfit.group==7),])
data.dtm.group7<-as.matrix(data.dtm[group7.name,])
freq7<- sort(colSums(data.dtm.group7), decreasing=TRUE)
head(freq7[!names(freq7) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
#get the eight group#
group8.name<-names(dtmfit.group[which(dtmfit.group==8),])
data.dtm.group8<-as.matrix(data.dtm[group8.name,])
freq8<- sort(colSums(data.dtm.group8), decreasing=TRUE)
head(freq8[!names(freq8) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
#get the nine group#
group9.name<-names(dtmfit.group[which(dtmfit.group==9),])
data.dtm.group9<-as.matrix(data.dtm[group9.name,])
freq9<- sort(colSums(data.dtm.group9), decreasing=TRUE)
head(freq9[!names(freq9) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
#get the ten group#
group10.name<-names(dtmfit.group[which(dtmfit.group==10),])
data.dtm.group10<-as.matrix(data.dtm[group10.name,])
freq10<- sort(colSums(data.dtm.group10), decreasing=TRUE)
head(freq10[!names(freq10) %in% c('advers','may','includ','affect','addit','requir','increas','chang','will','effect','result','oper','busi','financi','cost')], 20)
library(ggplot2)
library(scatterplot3d) 
library(sp)
group4<-data.frame(rate=data.dtm.group4[,"rate"],capital=data.dtm.group4[,"capit"],regulation=data.dtm.group4[,"regul"])
View(group4)
attach(group4)
s3d<-scatterplot3d(rate,capital,regulation, pch=16, highlight.3d=TRUE,type='h',main="3D Scatterplot")
text(s3d$xyz.convert(group4),labels=rownames(group4),cex=0.4,col="blue")
fit <- lm(regulation ~ rate+capital) 
s3d$plane3d(fit)
library(rgl)
plot3d(rate,capital,regulation, col="red", size=3)
library(Rcmdr)
scatter3d(rate,capital,regulation)




#############################
# < Your Name Here：Minxiang Pan >
# STAT W4240 
# Homework 04 
# < Homework Due Date >
#
# The following code analyzes the federalist papers
#############################

#################
# Setup
#################
library(tm)
install.packages("NLP")
library(NLP)
library(tm)
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
  # inspect to verify
  # inspect(fp[1])
  # another useful command
  # identical(fp[[1]], fp[["Federalist01.txt"]])
  # now let us iterate through and clean this up using tm functionality
  # make all words lower case
  fp = tm_map( fp , content_transformer(tolower));
  # remove all punctuation
  fp = tm_map( fp, removePunctuation);
  # remove stopwords like the, a, and so on. 
  fp = tm_map( fp, removeWords, stopwords("english"));
  # remove stems like suffixes
  fp = tm_map( fp, stemDocument)
  # remove extra whitespace
  fp = tm_map( fp, stripWhitespace)
  # now write the corpus out to the files for our future use.
  # MAKE SURE THE _CLEAN DIRECTORY EXISTS
  writeCorpus( fp , sprintf('%s_clean',dirname), filenames = names(fp))
}
# Clean the files
fp_hamilton_train_clean<-preprocess.directory("hw04/fp_hamilton_train")
fp_hamilton_test_clean<-preprocess.directory("hw04/fp_hamilton_test")
fp_madison_train_clean<-preprocess.directory("hw04/fp_madison_train")
fp_madison_test_clean<-preprocess.directory("hw04/fp_madison_test")
##########################################
#################
# Problem 1b
#################

##########################################
# To read in data from the directories:
# Partially based on code from C. Shalizi
read.directory <- function(dirname) {
  # Store the infiles in a list
  infiles = list();
  # Get a list of filenames in the directory
  filenames = dir(dirname,full.names=TRUE);
  for (i in 1:length(filenames)){
    infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
  }
  return(infiles)
}
hamilton.train<-read.directory("hw04/fp_hamilton_train_clean")
hamilton.test<-read.directory("hw04/fp_hamilton_test_clean")
madison.train<-read.directory("hw04/fp_madison_train_clean")
madison.test<-read.directory("hw04/fp_madison_test_clean")
##########################################
#################
# Problem 1c
#################

##########################################
# Make dictionary sorted by number of times a word appears in corpus 
# (useful for using commonly appearing words as factors)
# NOTE: Use the *entire* corpus: training, testing, spam and ham
make.sorted.dictionary.df <- function(infiles){
  # This returns a dataframe that is sorted by the number of times 
  # a word appears
  
  # List of vectors to one big vetor
  dictionary.full <- unlist(infiles) 
  # Tabulates the full dictionary
  tabulate.dic <- tabulate(factor(dictionary.full)) 
  # Find unique values
  dictionary <- unique(dictionary.full) 
  # Sort them alphabetically
  dictionary <- sort(dictionary)
  dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
  sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
  return(sort.dictionary.df)
}
all_infiles<-read.directory("hw04/all_cleaned")
dictionary.f<-make.sorted.dictionary.df(all_infiles)
##########################################
#################
# Problem 1d
#################

##########################################
# Make a document-term matrix, which counts the number of times each 
# dictionary element is used in a document
make.document.term.matrix <- function(infiles,dictionary){
  # This takes the text and dictionary objects from above and outputs a 
  # document term matrix
  num.infiles <- length(infiles);
  num.words <- nrow(dictionary);
  # Instantiate a matrix where rows are documents and columns are words
  dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
  for (i in 1:num.infiles){
    num.words.infile <- length(infiles[[i]]);
    infile.temp <- infiles[[i]];
    for (j in 1:num.words.infile){
      ind <- which(dictionary == infile.temp[j])[[1]];
      # print(sprintf('%s,%s', i , ind))
      dtm[i,ind] <- dtm[i,ind] + 1;
    }
  }
  return(dtm);
}
dtm.hamilton.train<-make.document.term.matrix(hamilton.train,dictionary.f)
dtm.hamilton.test<-make.document.term.matrix(hamilton.test,dictionary.f)
dtm.madison.train<-make.document.term.matrix(madison.train,dictionary.f)
dtm.madison.test<-make.document.term.matrix(madison.test,dictionary.f)

##########################################
#################
# Problem 1e
#################

##########################################
make.log.pvec <- function(dtm,mu){
  # Sum up the number of instances per word
  pvec.no.mu <- colSums(dtm)
  # Sum up number of words
  n.words <- sum(pvec.no.mu)
  # Get dictionary size
  dic.len <- length(pvec.no.mu)
  # Incorporate mu and normalize
  log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
  return(log.pvec)
}
mu=100/(nrow(dictionary.f))
logp.hamilton.train<-make.log.pvec(dtm.hamilton.train,mu)
logp.hamilton.test<-make.log.pvec(dtm.hamilton.test,mu)
logp.madison.train<-make.log.pvec(dtm.madison.train,mu)
logp.madison.test<-make.log.pvec(dtm.madison.test,mu)
##########################################
#################
# Problem 2
#################
##########################################
#Write the Naive Bayes classifier
prediction.result=list()
naive.bayes = function(logp.hamilton.train, logp.madison.train, log.prior.hamilton, log.prior.madison , dtm.test){
 for(i in 1:nrow(dtm.test)){
  post.hamilton=sum(logp.hamilton.train*dtm.test[i,])+log.prior.hamilton
  post.madison=sum(logp.madison.train*dtm.test[i,])+log.prior.madison
prediction.result[i]=list(prediction=ifelse(post.hamilton>post.madison,"H","M"))
}
return(prediction.result)
}
##########################################
#################
# Problem 3
#################
##########################################
log.prior.hamilton=log((length(hamilton.train))/(length(madison.train)+length(hamilton.train)))
log.prior.madison=log(length(madison.train))/(length(madison.train)+length(hamilton.train))
# Make the prediction using Naive Bayes Classifier
hamilton.test.list=naive.bayes(logp.hamilton.train,logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.hamilton.test)
madison.test.list=naive.bayes(logp.hamilton.train,logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.madison.test)
#The correct preditions for hamilton.test set
length(which(hamilton.test.list=="H"))
#The wrong preditions for hamilton.test set
length(which(hamilton.test.list=="M"))
#The correct preditions for madison.test set
length(which(madison.test.list=="M"))
#The wrong preditions for madison.test set
length(which(madison.test.list=="H"))
# The number of the infiles in both test sets
length(hamilton.test)
length(madison.test)
# The proportion of true positives
length(which(hamilton.test.list=="H"))/length(hamilton.test)
# The proportion of true negatives
length(which(madison.test.list=="M"))/length(madison.test)
#The proportion of false positives
length(which(madison.test.list=="H"))/length(madison.test)
#The proportion of false negatives
length(which(hamilton.test.list=="M"))/length(hamilton.test)
##########################################
#################
# Problem 4
#################
##########################################
#################
# Problem 4a
#################
##########################################
# The first way of sampling
#To merge the training sets into one set
install.packages("ttutils")
library(ttutils)
mix.train=merge(hamilton.train,madison.train)#merge the training sets
dtm.mix.train<-make.document.term.matrix(mix.train,dictionary.f)# make the document term matrix
# Creat the list to indicate the actual author of the training sets
mix.list=list()
mix.list[1:35]=c("H")
mix.list[36:50]=c("M")
#creat a dataframe to store the result
kfold.summary=data.frame(mu1=c(0,0,0),mu2=c(0,0,0),mu3=c(0,0,0),mu4=c(0,0,0),mu5=c(0,0,0))
rownames(kfold.summary)<-c("Correct-rate","False_negative_rate","False_positive_rate")
correct_rate=false_negative_rate=false_positive_rate=vector()
mu=c((1/nrow(dictionary.f)),(10/nrow(dictionary.f)),(100/nrow(dictionary.f)),(1000/nrow(dictionary.f)),(10000/nrow(dictionary.f)))
for(j in 1:5){
   mix.train_size=dim(dtm.mix.train)
   ntest = floor(mix.train_size [1]/5)
   ntrain= mix.train_size[1]- ntest
   correct_rate=false_negative_rate=false_positive_rate=vector()
   for(i in 1:5){
     ind_test=c(1: mix.train_size [1])[((mix.train_size [1]/5)*(i-1)+1): ((mix.train_size [1]/5)*i)]
     ind_train=c(1: mix.train_size [1])[- ind_test]
     dtm.mix.hamilton.train_temp= dtm.mix.train[ind_train[which(ind_train[1:length(ind_train)]<=35)],]
     dtm.mix.madison.train_temp= dtm.mix.train[ind_train[which(ind_train[1:length(ind_train)]>35)],]
     dtm.mix.test_temp= dtm.mix.train[ind_test[1:length(ind_test)],]
     logp.mix.hamilton.train<-make.log.pvec(dtm.mix.hamilton.train_temp,mu[j])
     logp.mix.madison.train<-make.log.pvec(dtm.mix.madison.train_temp,mu[j])
     log.prior.hamilton=length(which(ind_train[1:length(ind_train)]<=35))/40
     log.prior.madison=length(which(ind_train[1:length(ind_train)]>35))/40
     kfold.test.list=naive.bayes(logp.mix.hamilton.train, logp.mix.madison.train,log.prior.hamilton,log.prior.madison, dtm.mix.test_temp)
     correct_rate[i]=mean(unlist(kfold.test.list)==unlist(mix.list[ind_test[1:length(ind_test)]]))
     if(length(which(ind_test[1:length(ind_test)]<=35))==0){false_negative_rate[i]=0}
     else{false_negative_rate[i]=length(which(kfold.test.list=="M" & unlist(kfold.test.list)!=unlist(mix.list[ind_test[1:length(ind_test)]])))/length(which(ind_test[1:length(ind_test)]<=35))}
     if(length(which(ind_test[1:length(ind_test)]>35))==0){false_positive_rate[i]=0}
     else{false_positive_rate[i]=length(which(kfold.test.list=="H" & unlist(kfold.test.list)!=unlist(mix.list[ind_test[1:length(ind_test)]])))/length(which(ind_test[1:length(ind_test)]>35))}

     }
   kfold.summary[1,j]=sum(correct_rate)/5
   kfold.summary[2,j]=sum(false_negative_rate)/5
   kfold.summary[3,j]=sum(false_positive_rate)/5
     }
 fix(kfold.summary)
# plot the result
par(mfrow=c(3,1))
plot(log(mu),kfold.summary[1,],ylab="Correction Rate")
title("Mu vs Correction Rate")
plot(log(mu),kfold.summary[2,],ylab="False Negative Rate")
title("Mu vs False Negative Rate")
plot(log(mu),kfold.summary[3,],ylab="False Positive Rate")
title("Mu vs False Positive Rate")

# The second way of sampling
kfold.summary2=data.frame(mu1=c(0,0,0),mu2=c(0,0,0),mu3=c(0,0,0),mu4=c(0,0,0),mu5=c(0,0,0))
rownames(kfold.summary2)<-c("Correct-rate","False_negative_rate","False_positive_rate")
# use loop to realize k-fold cross validation
mix.list1=list()
mix.list1[1:7]=c("H")
mix.list1[8:10]=c("M")
correct_rate=false_negative_rate=false_positive_rate=vector()
mu=c((1/nrow(dictionary.f)),(10/nrow(dictionary.f)),(100/nrow(dictionary.f)),(1000/nrow(dictionary.f)),(10000/nrow(dictionary.f)))
for(j in 1:5){

  correct_rate=false_negative_rate=false_positive_rate=vector()
  for(i in 1:5){
    dtm.mix.hamilton.train_temp= dtm.hamilton.train[-((7*(i-1)+1):(7*i)),]
    dtm.mix.madison.train_temp= dtm.madison.train[-((3*(i-1)+1):(3*i)),]
    dtm.mix.test_temp= rbind(dtm.hamilton.train[(7*(i-1)+1):(7*i),],dtm.madison.train[(3*(i-1)+1):(3*i),])
    logp.mix.hamilton.train<-make.log.pvec(dtm.mix.hamilton.train_temp,mu[j])
    logp.mix.madison.train<-make.log.pvec(dtm.mix.madison.train_temp,mu[j])
    log.prior.hamilton=0.7
    log.prior.madison=0.3
    kfold.test.list=naive.bayes(logp.mix.hamilton.train, logp.mix.madison.train,log.prior.hamilton,log.prior.madison, dtm.mix.test_temp)
    correct_rate[i]=mean(unlist(kfold.test.list)==unlist(mix.list1[1:length(mix.list1)]))
   false_negative_rate[i]=length(which(kfold.test.list=="M" & unlist(kfold.test.list)!=unlist(mix.list1[1:length(mix.list1)])))/7
   false_positive_rate[i]=length(which(kfold.test.list=="H" & unlist(kfold.test.list)!=unlist(mix.list1[1:length(mix.list1)])))/3
  }
  kfold.summary2[1,j]=sum(correct_rate)/5
  kfold.summary2[2,j]=sum(false_negative_rate)/5
  kfold.summary2[3,j]=sum(false_positive_rate)/5
}
fix(kfold.summary2)
# plot the result
par(mfrow=c(3,1))
plot(log(mu),kfold.summary2[1,],ylab="Correction Rate")
title("Mu vs Correction Rate")
plot(log(mu),kfold.summary2[2,],ylab="False Negative Rate")
title("Mu vs False Negative Rate")
plot(log(mu),kfold.summary2[3,],ylab="False Positive Rate")
title("Mu vs False Positive Rate")
##########################################
#################
# Problem 4c
#################
##########################################
mix.test=merge(hamilton.test,madison.test)
dtm.mix.test<-make.document.term.matrix(mix.test,dictionary.f)

#creat a dataframe to store the result
test.summary=data.frame(mu1=c(0,0,0),mu2=c(0,0,0),mu3=c(0,0,0),mu4=c(0,0,0),mu5=c(0,0,0))
rownames(test.summary)<-c("Correct-rate","False_negative_rate","False_positive_rate")
# Creat the list to indicate the actual author of the training sets
mix.test.list=list()
mix.test.list[1:16]=c("H")
mix.test.list[17:27]=c("M")
# Use loop to compute the result
mu=c((1/nrow(dictionary.f)),(10/nrow(dictionary.f)),(100/nrow(dictionary.f)),(1000/nrow(dictionary.f)),(10000/nrow(dictionary.f)))
for(j in 1:5){
    logp.full.hamilton.train<-make.log.pvec(dtm.mix.train[1:35,],mu[j])
    logp.full.madison.train<-make.log.pvec(dtm.mix.train[36:50,],mu[j])
    log.prior.hamilton=0.7
    log.prior.madison=0.3
    test.list=naive.bayes(logp.full.hamilton.train, logp.full.madison.train,log.prior.hamilton,log.prior.madison, dtm.mix.test)
    test.summary[1,j]=mean(unlist(test.list)==unlist(mix.test.list[1:length(mix.test.list)]))
    test.summary[2,j]=length(which(test.list=="M" & unlist(test.list)!=unlist(mix.test.list[1:length(mix.test.list)])))/length(which(mix.test.list[1:length(mix.test.list)]=="H"))
    test.summary[3,j]=length(which(test.list=="H" & unlist(test.list)!=unlist(mix.test.list[1:length(mix.test.list)])))/length(which(mix.test.list[1:length(mix.test.list)]=="M"))
  
}
fix(test.summary)
# plot the result
par(mfrow=c(3,1))
plot(log(mu),test.summary[1,],ylab="Correction Rate")
title("Mu vs Correction Rate")
plot(log(mu),test.summary[2,],ylab="False Negative Rate")
title("Mu vs False Negative Rate")
plot(log(mu),test.summary[3,],ylab="False Positive Rate")
title("Mu vs False Positive Rate")


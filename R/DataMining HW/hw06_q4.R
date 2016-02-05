#############################
# < Your Name Here：Minxiang Pan >
# STAT W4240 
# Homework 05
# < Homework Due Date >
#
#############################
#################
# Question 4 (a)
#################
# as the previous homework we center and scale the training data and the test data
#The first way is the majority vote approach:
# we cited the code in the HW04 to create the new traing sets and test sets
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
preprocess.directory('fp_hamilton_test')
preprocess.directory('fp_hamilton_train')
preprocess.directory('fp_madison_test')
preprocess.directory('fp_madison_train')
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
hamilton.train <- read.directory('fp_hamilton_train_clean')
hamilton.test <- read.directory('fp_hamilton_test_clean')
madison.train <- read.directory('fp_madison_train_clean')
madison.test <- read.directory('fp_madison_test_clean')
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

doc.all <- c(hamilton.train,hamilton.test,madison.train,madison.test)
dictionary <- make.sorted.dictionary.df(doc.all)
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
      #print(c(i,j))
    }
  }
  return(dtm);
}

dtm.hamilton.train <- make.document.term.matrix(hamilton.train,dictionary)
dtm.hamilton.test <- make.document.term.matrix(hamilton.test,dictionary)
dtm.madison.train <- make.document.term.matrix(madison.train,dictionary)
dtm.madison.test <- make.document.term.matrix(madison.test,dictionary)

##################
# creat two new dataframe for training set and test set
train.frame=as.data.frame(rbind(dtm.hamilton.train,dtm.madison.train ))
names(train.frame)=as.vector(dictionary$word)
test.frame=as.data.frame(rbind(dtm.hamilton.test,dtm.madison.test ))
names(test.frame)=as.vector(dictionary$word)
test.list=train.list=vector()
train.list[1:35]=1
train.list[36:50]=0
test.list[1:16]=1
test.list[17:27]=0
train.list=as.factor(train.list)
test.list=as.factor(test.list)
train.frame=cbind(train.frame,data.frame(Author=train.list))
test.frame=cbind(test.frame,data.frame(Author=test.list))
# we scale the training data first
x=scale(train.frame[,-length(train.frame)])
x[is.na(x)]=0
y=train.frame$Author
# we use train mean and train sd to scale the testing data
# we cite the code in the hw05 solution
train.mean=apply(train.frame[,-length(train.frame)],2,mean)
train.sd=apply(train.frame[,-length(train.frame)],2,sd)
t=scale(test.frame[,-length(test.frame)],center=train.mean,scale=train.sd)
t[is.na(t)]=0
#set up 
library(e1071)
#train the model
tune.out=tune(svm,Author~.,data=cbind(x[,1:100],data.frame(Author=y)),kernel="linear",type="C",scale=FALSE,ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
#make prediction
bestmod=tune.out$best.model
bestmod.pred=predict(bestmod,cbind(t[,1:100],data.frame(Author=test.frame$Author)))
#show the predicting result
table(bestmod.pred,test.frame$Author)
#compute the rate of correct classification 
mean(bestmod.pred==test.frame$Author)
#################
# Question 4 (b)
#################
#use loop to make predictions for times 
correction.rate=vector()
for(i in 1:20)
{
  tune.out=tune(svm,Author~.,data=cbind(x[,1:(5*i)],data.frame(Author=y)),kernel="linear",type="C",scale=FALSE,ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
  bestmod=tune.out$best.model
  bestmod.pred=predict(bestmod,cbind(t[,1:(5*i)],data.frame(Author=test.frame$Author)))
  correction.rate[i]=mean(bestmod.pred==test.frame$Author)
}
#plot the number of words against the rate of classification correction
n.words=seq(5,100,by=5)
plot(n.words,correction.rate,xlab="n.words",ylab="correction.rate",type="b")
points(which.max(correction.rate)*5,correction.rate[which.max(correction.rate)],col="red")
title("Number of words vs Correction rate")
# locate the plot
correction.rate[which.max(correction.rate)]
which.max(correction.rate)*5
#################
# Question 4 (c)
#################
# we use radial kernel in this part
correction.rate=vector()
for(i in 1:20)
{
  tune.out=tune(svm,Author~.,data=cbind(x[,1:(5*i)],data.frame(Author=y)),kernel="radial",type="C",scale=FALSE,ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)),gamma=c(0.5,1,2,3,4))
  bestmod=tune.out$best.model
  bestmod.pred=predict(bestmod,cbind(t[,1:(5*i)],data.frame(Author=test.frame$Author)))
  correction.rate[i]=mean(bestmod.pred==test.frame$Author)
}
#plot the number of words against the rate of classification correction
n.words=seq(5,100,by=5)
plot(n.words,correction.rate,xlab="n.words",ylab="correction.rate",type="b")
points(which.max(correction.rate)*5,correction.rate[which.max(correction.rate)],col="red")
title("Number of words vs Correction rate")
# locate the plot
correction.rate[which.max(correction.rate)]
which.max(correction.rate)*5
#################
# Question 4 (d)
#################
#find the corresponding columns of the two important words
x1=cbind(x[,which(colnames(train.frame)=="depart")],x[,which(colnames(train.frame)=="upon")])
t1=cbind(t[,which(colnames(test.frame)=="depart")],t[,which(colnames(test.frame)=="upon")])
tune.out=tune(svm,Author~.,data=cbind(x1,data.frame(Author=y)),kernel="radial",type="C",scale=FALSE,ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)),gamma=c(0.5,1,2,3,4))
bestmod=tune.out$best.model
bestmod.pred=predict(bestmod,cbind(t1,data.frame(Author=test.frame$Author)))
#plot the result
table(bestmod.pred,test.frame$Author)
mean(bestmod.pred==test.frame$Author)

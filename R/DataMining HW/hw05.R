#############################
# < Your Name Here：Minxiang Pan >
# STAT W4240 
# Homework 05
# < Homework Due Date >
#
#############################

#################
# Question 4
#################
# create a sequence of propotion
m1=seq(0,1,by=0.01)
#caculate the Gini Index
g=as.list(2*m1*(1-m1))
# use function to caculate Entropy
c_ent=function(m1){
  return(-((m1*log(m1))+((1-m1)*log(1-m1))))
}
e=sapply(m1,c_ent)
# use function to caculate Classification Error
c=1-pmax(m1,1-m1)
c=sapply(m1,class_err)
d=data.frame(Cross.Entropy=e,Classification.error=c)
# plot the data 
plot(m1,g,col="black",type="l",ylab="values",xlab="m1")
# use matlines function to plot the data in the dataframe
matlines(m1,d,col=c("blue","red"))
#add a legend
legend("topleft",c("Gini Index","Cross Entropy","Classification Error"),col=c("black","blue","red"),lty=c(1,1,2),bty="n",cex=0.9)
title("m1 vs Gini.index Cross Entropy Classification Error")
#################
# Question 5
#################
#The first way is the majority vote approach:
a=c(0.1,0.15,0.2,0.2,0.55,0.6,0.6,0.65,0.7,0.75)
print(ifelse(mean(a>0.5)>0.5,"Red","Green"))
#The second way to classify by averaging the probability 
print(ifelse(mean(a)>0.5,"Red","Green"))
#################
# Question 6(a)
#################
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
train.frame=data.frame(rbind(dtm.hamilton.train,dtm.madison.train ))
names(train.frame)=as.vector(dictionary$word)
test.frame=data.frame(rbind(dtm.hamilton.test,dtm.madison.test ))
names(test.frame)=as.vector(dictionary$word)
test.list=train.list=vector()
train.list[1:35]=1
train.list[36:50]=0
test.list[1:16]=1
test.list[17:27]=0
train.list=as.factor(train.list)
test.list=as.factor(test.list)
train.frame=cbind(train.frame,data.frame(Author=train.list))
test.frame=cbind(test.frame,data.frame(Author=test.list)
# use package "rpart"
install.packages("rpart")
library(rpart)
#using tree to make regression with Gini impurity coefficient splits
fit.train=rpart(train.frame$Author~.,train.frame,method="class")
plot(fit.train);text(fit.train, use.n=TRUE)
author.predGini=predict(fit.train,test.frame,type="class")
plot(author.predGini)
table(author.predGini,test.frame$Author)
# correct classification rate
mean(author.predGini==test.frame$Author)
#false negative rate
false.negative=length(which(author.predGini!=test.frame$Author & author.predGini==0))/length(which(test.frame$Author==1))
#false positive rate
false.positive=length(which(author.predGini!=test.frame$Author & author.predGini==1))/length(which(test.frame$Author==0))
#################
# Question 6(b)
#################
#using tree to make regression with information gain splits
fit.train1=rpart(train.frame$Author~.,train.frame,method="class",parms=list(split='information'))
plot(fit.train1);text(fit.train1, use.n=TRUE)
author.predInfo=predict(fit.train1,test.frame,type="class")
table(author.predInfo,test.frame$Author)
# correct classification rate
mean(author.predInfo==test.frame$Author)
#false negative rate
false.negative1=length(which(author.predInfo!=test.frame$Author & author.predInfo==0))/length(which(test.frame$Author==1))
#false positive rate
false.positive1=length(which(author.predInfo!=test.frame$Author & author.predInfo==1))/length(which(test.frame$Author==0))
#################
# Question 7(b)
#################
# use ridge regression
install.packages("glmnet")
library(Matrix)
library(glmnet)
scale.data=scale(rbind(train.frame[,-length(train.frame)],test.frame[,-length(test.frame)]),center=T,scale=T)
x=scale.data[1:50,]
x=scale(x,center=T,scale=T)
x[is.na(x)]=0
y=train.frame$Author
fit.ridge=glmnet(x,y,family="binomial",alpha=0)
plot(fit.ridge)
cv.fit.ridge=cv.glmnet(x,y,family="binomial",alpha=0)
t=scale.data[51:77,]
t[is.na(t)]=0
cv.ridge.pred=predict(cv.fit.ridge,t,type="class")
fix(cv.ridge.pred)
table(cv.ridge.pred,test.frame$Author)
# correct classification rate
mean(cv.ridge.pred==test.frame$Author)
#false negative rate
false.negative2=length(which(cv.ridge.pred!=test.frame$Author & cv.ridge.pred==0))/length(which(test.frame$Author==1))
#false positive rate
false.positive2=length(which(cv.ridge.pred!=test.frame$Author & cv.ridge.pred==1))/length(which(test.frame$Author==0))
#find the best lambda
which(cv.fit.ridge$lambda==cv.fit.ridge$lambda.min)
# list the most important 10 covariates
sort(abs(fit.ridge$beta[,which(cv.fit.ridge$lambda==cv.fit.ridge$lambda.min)]),decreasing=T)[1:10]
#################
# Question 7(c)
#################
# use lasso regression
fit.lasso=glmnet(x,y,family="binomial",alpha=1)
plot(fit.lasso)
cv.fit.lasso=cv.glmnet(x,y,family="binomial",alpha=1)
cv.lasso.pred=predict(cv.fit.lasso,t,type="class")
fix(cv.lasso.pred)
table(cv.lasso.pred,test.frame$Author)
# correct classification rate
mean(cv.lasso.pred==test.frame$Author)
#false negative rate
false.negative3=length(which(cv.lasso.pred!=test.frame$Author & cv.lasso.pred==0))/length(which(test.frame$Author==1))
#false positive rate
false.positive3=length(which(cv.lasso.pred!=test.frame$Author & cv.lasso.pred==1))/length(which(test.frame$Author==0))
#find the best lambda
which(cv.fit.lasso$lambda==cv.fit.lasso$lambda.min)
# list the most important 10 covariates
sort(abs(fit.lasso$beta[,which(cv.fit.lasso$lambda==cv.fit.lasso$lambda.min)]),decreasing=T)[1:10]




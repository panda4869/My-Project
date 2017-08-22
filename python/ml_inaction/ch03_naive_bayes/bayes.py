
import numpy as np

def loadDataSet():
    postingList=[['my', 'dog', 'has', 'flea', 'problems', 'help', 'please'],
                 ['maybe', 'not', 'take', 'him', 'to', 'dog', 'park', 'stupid'],
                 ['my', 'dalmation', 'is', 'so', 'cute', 'I', 'love', 'him'],
                 ['stop', 'posting', 'stupid', 'worthless', 'garbage'],
                 ['mr', 'licks', 'ate', 'my', 'steak', 'how', 'to', 'stop', 'him'],
                 ['quit', 'buying', 'worthless', 'dog', 'food', 'stupid']]
    classVec = [0,1,0,1,0,1]    #1 is abusive, 0 not
    return postingList,classVec


def createVocabList(dataSet):
    vocab=set([])
    for voc in dataSet:
        vocab=vocab.union(set(voc))
    return list(vocab)










def setOfWords2Vec(vocabList, inputSet):
    inputindex=[0]*len(vocabList)
    for word in inputSet:
        if word in vocabList:
            inputindex[vocabList.index(word)]=1
        else: 
            print('Word %s is not in the list'%word)
    return inputindex


def trainNB0(trainMatrix,trainCategory):
    numDocument=len(trainMatrix)
    numWord=len(trainMatrix[0])
    p1b=sum(trainCategory)/numDocument
    p1num=np.ones(numWord)
    p0num=np.ones(numWord)
    p1Denom=2.0
    p0Denom=2.0
    for i in range(numDocument):
        if trainCategory[i]==1:
            p1num+=trainMatrix[i]
            p1Denom+=sum(trainMatrix[i])
        else:
            p0num+=trainMatrix[i]
            p0Denom+=sum(trainMatrix[i])
    p0v=np.log(p0num/p0Denom)
    p1v=np.log(p1num/p1Denom)
    return p0v,p1v,p1b


def classifyNB(vec2Classify, p0Vec, p1Vec, pClass1):
    p1=sum(vec2Classify*p1Vec)+np.log(pClass1)
    p0=sum(vec2Classify*p0Vec)+np.log(1-pClass1)
    if p1>p0:
        return 1
    else:
        return 0


def testingNB():
    dataSet,labels=loadDataSet() 
    voclist=createVocabList(dataSet)
    trainM=[]
    for doc in dataSet:
        trainM.append(setOfWords2Vec(voclist,doc))

    p0V,p1V,pAb=trainNB0(trainM,labels)
    testEntry = ['love', 'my', 'dalmation']
    thisDoc=setOfWords2Vec(voclist,testEntry)
    print (testEntry,'classified as: ',classifyNB(thisDoc,p0V,p1V,pAb))

    testEntry = ['stupid', 'garbage']
    thisDoc=setOfWords2Vec(voclist,testEntry)
    print (testEntry,'classified as: ',classifyNB(thisDoc,p0V,p1V,pAb))


def bagOfWords2VecMN(vocabList, inputSet):
    inputindex=[0]*len(vocabList)
    for word in inputSet:
        if word in vocabList:
            inputindex[vocabList.index(word)]+=1
    return inputindex

def textParse(bigString): 
    import re
    regex=re.compile('\\W+')
    listofword=re.split(regex,bigString)
    listofword=[lw.lower() for lw in listofword if len(lw)>=1]
    return listofword

def spamTest():
    import random
    import codecs
    docList=[]; classList = []; fullText =[]
    for i in range(1,26):
        filename_ham=str(i)+'.txt'
        wordlist=textParse(open('email/ham/'+filename_ham,encoding='utf-8',errors='ignore').read())
        docList.append(wordlist)
        fullText.extend(wordlist)
        classList.append(0)
        filename_spam=str(i)+'.txt'
        wordlist=textParse(open('email/spam/'+filename_spam,encoding='utf-8',errors='ignore').read())
        docList.append(wordlist)
        fullText.extend(wordlist)
        classList.append(1)
    voclist=createVocabList(docList)
    trainM=[]
    for doc in docList:
        trainM.append(bagOfWords2VecMN(voclist,doc))
    trainSet=list(range(50))
    testSet=[]
    trainclass=[]
    testclass=[]
    tm=[]
    for i in range(10):
        testsetindex=int(random.uniform(0,len(trainSet)))
        del trainSet[testsetindex]
        testSet.append(testsetindex)
    for trainindex in trainSet:
        trainclass.append(classList[trainindex])
        tm.append(trainM[trainindex])
    #print(trainSet)
    #print(testSet)
    p0V,p1V,pAb=trainNB0(tm,trainclass)
    errorCount=0
    for test in testSet:
        if classifyNB(trainM[test],p0V,p1V,pAb)!=classList[test]:
            errorCount+=1
            print('Classification Error is ', docList[test])
    #print(errorCount)
    print ('the error rate is: ',float(errorCount)/len(testSet))




if __name__=='__main__':
    #testingNB()
    
    #print(voclist)
    #print(dataSet[0])
    #print(setOfWords2Vec(voclist,dataSet[0]))

    #mytext='This is the best python book for M.L. '
    #print(textParse(mytext))
    spamTest()

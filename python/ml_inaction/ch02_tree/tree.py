from math import log
import operator

def createDataSet():
    dataSet=[[1,1,'yes'],
    [1,1,'yes'],
    [1,0,'no'],
    [0,1,'no'],
    [0,1,'no']]
    labels=['no surfacing','flippers']
    return dataSet,labels

def calcShannonEnt(dataSet):
    labelcount=dict()
    shannoEnt=0
    for featvec in dataSet:
        currentlabel=featvec[-1]
        if currentlabel not in labelcount.keys():
            labelcount[currentlabel]=0
        labelcount[currentlabel]+=1
    for key in labelcount.keys():
        prob=float(labelcount[key])/(len(dataSet))
        shannoEnt-=prob*log(prob,2)

    return shannoEnt




def splitDataSet(dataSet, axis, value):
    
    reducedataset=[]
    for featvec in dataSet:
        reducefeat=[]
        try:
            if featvec[axis]==value:
                reducefeat.extend(featvec[:axis])
                reducefeat.extend(featvec[axis+1:])
                reducedataset.append(reducefeat)
        except:
            print('Exceed Num of Feature')
    return reducedataset



def chooseBestFeatureToSplit(dataSet):
    baseEntropy=calcShannonEnt(dataSet)
    bestInfoGain=-1
    bestfeature=0
    for i in range(len(dataSet[0])-1):
        featlist=[example[i] for example in dataSet]
        uniquefeat=set(featlist)
        #print(uniquefeat)
        newEntropy=0
        for value in uniquefeat:
            subSet=splitDataSet(dataSet,i,value)
            prob=float(len(subSet))/len(dataSet)
            newEntropy+=prob*calcShannonEnt(subSet)

        info_gain=baseEntropy-newEntropy
        if info_gain>bestInfoGain:
            bestInfoGain=info_gain
            bestfeature=i
    return bestfeature



def majorityCnt(classList):
    classCount=dict()
    for vote in classList:
        if vote not in classCount.keys():
            classCount[vote]=0
        classCount[vote]+=1

    return sorted(classCount.items(),key=operator.itemgetter(1),reverse=True)[0][0]


def createTree(dataSet,labels):
    classlist=[example[-1] for example in dataSet]
    if classlist.count(classlist[0])==len(classlist):
        return classlist[0]
    if len(dataSet[0])==1:
        return majorityCnt(classlist)
    bestfeature=chooseBestFeatureToSplit(dataSet)
    bestfeaturelabel=labels[bestfeature]
    mytree={bestfeaturelabel:{}}
    featvec=[example[bestfeature] for example in dataSet]
    uniquefeat=set(featvec)
    del (labels[bestfeature])
    for value in uniquefeat:
        subSet=labels[:]
        mytree[bestfeaturelabel][value]=createTree(splitDataSet(dataSet,bestfeature,value),subSet)
    return mytree


def classify(inputTree,featLabels,testVec):
    firststr=list(inputTree.keys())[0]
    secondDict=inputTree[firststr]
    firstfeatindex=featLabels.index(firststr)
    for key in secondDict.keys():
        if testVec[firstfeatindex]==key:
            if type(secondDict[key]).__name__=='dict':
                classlabel=classify(secondDict[key],featLabels,testVec)
            else:
                classlabel=secondDict[key]
            return classlabel




if __name__=='__main__':
    dataSet,labels=createDataSet()
    #cl=[example[-1] for example in dataSet]
    #print(dataSet)
    #print(calcShannonEnt(dataSet))
    #print(splitDataSet(dataSet,0,1))
    #print(splitDataSet(dataSet,0,0))
    #print (createTree(dataSet,labels))
    #print (chooseBestFeatureToSplit(dataSet))
    #print(labels)
    #mytree=createTree(dataSet,labels)
    #dataSet,labels=createDataSet()
    #print(classify(mytree,labels,[1,0]))
    f=open('lenses.txt','r')
    lenses=[inv.strip().split('\t') for inv in f.readlines()]
    f.close()
    lenseLabels=['age','prescript','astigmatic','tearRate']
    #print (lenses)
    mytree=createTree(lenses,lenseLabels)
    print(mytree)





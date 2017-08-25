from numpy import *

def loadDataSet(filename):
    import os
    path=os.getcwd()+'/'+filename
    fr=open(path,'r')
    dataArr=[]
    for line in fr.readlines():
        currentLine=line.strip().split('\t')
        #print(currentLine)
        lineToAppend=list(map(float,currentLine))
        dataArr.append(lineToAppend)
    dataMat=mat(dataArr)
    return dataMat


def binSplitDataSet(dataSet,feature,value):
    mat0=dataSet[nonzero(dataSet[:,feature]>value)[0],:]
    mat1=dataSet[nonzero(dataSet[:,feature]<=value)[0],:]
    return mat0,mat1

def regLeaf(dataSet):
    return mean(dataSet[:,-1])

def regError(dataSet):
    return var(dataSet[:,-1])*shape(dataSet)[0]


def chooseBestSplit(dataSet,leafType=regLeaf,errorType=regError,ops=(1,4)):
    tolS=ops[0] ; tolN=ops[1]
    m,n=shape(dataSet)
    if len(set(dataSet[:,-1].T.tolist()[0]))==1:
        return None,leafType(dataSet)

    S=errorType(dataSet)
    bestS=inf;bestValue=0;bestIndex=0
    for featIndex in range(n-1):
        #print (shape(dataSet[:,featIndex].T.tolist()))
        #print(set(dataSet[:,featIndex].T.tolist()[0]))
        for splitValue in set(dataSet[:,featIndex].T.tolist()[0]):
            mat0,mat1=binSplitDataSet(dataSet,featIndex,splitValue)
            if (shape(mat0)[0]<tolN) or (shape(mat1)[0]<tolN): continue
            errorS=errorType(mat0)+errorType(mat1)
            if errorS<bestS:
                bestS=errorS
                bestIndex=featIndex
                bestValue=splitValue
    if (S-bestS)<tolS:
        return None, leafType(dataSet)
    mat0,mat1=binSplitDataSet(dataSet,bestIndex,bestValue)
    if (shape(mat0)[0]<tolN) or (shape(mat1)[0]<tolN):
        return None,leafType(dataSet)
    return bestIndex,bestValue

def creatTree(dataSet,leafType=regLeaf,errorType=regError,ops=(1,4)):
    feat,val=chooseBestSplit(dataSet,leafType,errorType,ops)
    if feat==None: return val
    retTree={}
    retTree['spInd']=feat
    retTree['spVal']=val
    lsp,rsp=binSplitDataSet(dataSet,feat,val)
    retTree['left']=creatTree(lsp,leafType,errorType,ops)
    retTree['right']=creatTree(rsp,leafType,errorType,ops)
    return retTree


def isTree(obj):
    return type(obj).__name__=='dict'

def getMean(tree):
    if isTree(tree['left']):tree['left']=getMean(tree['left'])
    if isTree(tree['right']): tree['right']=getMean(tree['right'])
    return (tree['left']+tree['right'])/2.0
def prune(tree,testSet):
    if shape(testSet)[0]==0: return getMean(tree)
    if (isTree(tree['left'])) or (isTree(tree['right'])):
        lset,rset=binSplitDataSet(testSet,tree['spInd'],tree['spVal'])
    if isTree(tree['left']):tree['left']=prune(tree['left'],lset)
    if isTree(tree['right']):tree['right']=prune(tree['right'],rset)
    if not isTree(tree['left']) and not isTree(tree['right']):
        lset,rset=binSplitDataSet(testSet,tree['spInd'],tree['spVal'])
        ErrorSplit=sum(power(lset[:,-1]-tree['left'],2))+sum(power(rset[:,-1]-tree['right'],2))
        treeMean=(tree['left']+tree['right'])/2.0
        ErrorComb=sum(power(testSet[:,-1]-treeMean,2))
        #print(rset)
        #treeMean=(lset[:,-1]+rset[:,-1])/2.0
        if ErrorComb<ErrorSplit:
            print ('Merging')
            return treeMean
        else:
            return tree
    else: return tree


def linearSolver(dataSet):
    dataMat=mat(dataSet)
    m,n=shape(dataMat)
    X=mat(ones((m,n)));Y=mat(ones((m,1)))
    X[:,1:n]=dataMat[:,0:n-1];Y=dataMat[:,-1]
    xTx=X.T*X
    if linalg.det(xTx)==0:
        print('Can not inverse' )
        return 0
    ws=xTx.I*(X.T*Y)
    return ws,X,Y

def modelLeaf(dataSet):
    ws,X,Y=linearSolver(dataSet)
    return ws

def modelError(dataSet):
    ws,X,Y=linearSolver(dataSet)
    yHat=X*ws
    return sum(power(Y-yHat,2))

def regTreeEval(model,inData):
    return float(model)


def modelTreeEval(model,inData):
    n=shape(inData)[1]
    X=mat(ones((1,n+1)))
    X[:,1:n+1]=inData
    return float(X*model)
def treeForeCast(tree,inData,modelEval=regTreeEval):
    if not isTree(tree): return modelEval(tree,inData)
    if inData[tree['spInd']]>tree['spVal']:
        if isTree(tree['left']):
            return treeForeCast(tree['left'],inData,modelEval)
        else:
            return modelEval(tree['left'],inData)
    else:
        if isTree(tree['right']):
            return treeForeCast(tree['right'],inData,modelEval)
        else:
            return modelEval(tree['right'],inData)


def createForeCast(tree,testData,modelEval=regTreeEval):
    m,n=shape(testData)
    yHat=mat(zeros((m,1)))
    for i in range(m):
        yHat[i,0]=treeForeCast(tree,testData[i],modelEval)
    return yHat




if __name__=='__main__':
    #myMat2=loadDataSet('ex2.txt')
    #myMatTree=creatTree(myMat2,ops=(0,1))
    #print(myMatTree)
    #myMatTest=loadDataSet('ex2test.txt')
    #treePrune=prune(myMatTree,mat(myMatTest))
    #print(treePrune)
    #print(creatTree(myMat2,leafType=modelLeaf,errorType=modelError,ops=(1,10)))
    trainMat=mat(loadDataSet('bikeSpeedVsIq_train.txt'))
    testMat=mat(loadDataSet('bikeSpeedVsIq_test.txt'))
    mytree=creatTree(trainMat,leafType=modelLeaf,errorType=modelError,ops=(1,20))
    yHat=createForeCast(mytree,testMat[:,0],modelTreeEval)
    print(corrcoef(yHat.T,testMat[:,1].T)[0,1])
    #print(corrcoef(yHat,testMat[:,1]))



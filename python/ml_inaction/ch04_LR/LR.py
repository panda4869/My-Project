from numpy import *


def loadDataSet():
    dataMat=[]; labelMat=[]
    fr=open('testSet.txt','r')
    for line in fr.readlines():
        element=line.strip().split()
        dataMat.append([1,float(element[0]),float(element[1])])
        labelMat.append(int(element[2]))
    return dataMat,labelMat

def sigmoid(inx):
    return 1/(1+exp(-inx))

def gradAscent(dataMatIn, classLabels):
    dataMatrix=mat(dataMatIn)
    classMatrix=mat(classLabels).transpose()
    n,m=shape(dataMatrix)
    maxiter=500
    weights=ones((m,1))
    alpha=0.001
    for i in range(maxiter):
        h=sigmoid(dataMatrix*weights)
        #print(h)
        error=classMatrix-h
        #print(error)
        weights=weights+alpha*dataMatrix.transpose()*error

    return weights

def plotBestFit(weights):
    import matplotlib.pyplot as plt
    Data,Label=loadDataSet()
    Data=array(Data)
    n,m=shape(mat(Data))
    xcord1=[];ycord1=[]
    xcord2=[];ycord2=[]
    for i in range(n):
        if Label[i]==1:
            xcord1.append(Data[i,1])
            ycord1.append(Data[i,2])
        else:
            xcord2.append(Data[i,1])
            ycord2.append(Data[i,2])  
    #print(xcord1)
    f,ax=plt.subplots()
    ax.scatter(xcord1,ycord1,s=30,color='red',marker='o')
    ax.scatter(xcord2,ycord2,s=30,color='green',marker='x')
    x=arange(-3,3,0.1)
    weights=ravel(weights)
    y=(-weights[0]-weights[1]*x)/weights[2]
    #print(x,y)
    plt.plot(x,y)
    plt.xlabel('x1')
    plt.ylabel('x2')
    plt.title('LR Graph')
    plt.show()


def stocGradAscent0(dataMatrix, classLabels):
    dataMatrix=array(dataMatrix)
    m,n=shape(dataMatrix)
    weights=ones(n)
    alpha=0.01
    for i in range(m):
        h=sigmoid(sum(dataMatrix[i]*weights))
        error=classLabels[i]-h
        #print(error)
        #print(error*dataMatrix[i])
        weights=weights+alpha*error*dataMatrix[i]

    return weights


def stocGradAscent1(dataMatrix, classLabels, numIter=150):
    import random
    dataMatrix=array(dataMatrix)   
    m,n=shape(dataMatrix)
    weights=ones(n)
    for j in range(numIter):
        dataIndex=list(range(m))
        for i in range(m):
            alpha=4.0/(1.0+i+j)+0.01
            randomIndex=int(random.uniform(0,len(dataIndex)))
            h=sigmoid(sum(dataMatrix[randomIndex]* weights))
            error=classLabels[randomIndex]-h
            weights=weights+alpha*error*dataMatrix[randomIndex]
            del dataIndex[randomIndex]
    return weights


def classifyVector(inX, weights):
    prob=sigmoid(sum(inX*weights))
    if prob>0.5:
        return 1.0
    else:
        return 0.0


def colicTest():
    frTrain = open('horseColicTraining.txt'); frTest = open('horseColicTest.txt')
    trainingSet=[]
    trainingLabels=[]
    for line in frTrain.readlines():
        currentline=line.strip().split('\t')
        lineArr=[]
        for i in range(21):
            lineArr.append(float(currentline[i]))
        trainingSet.append(lineArr)
        trainingLabels.append(float(currentline[21]))

    weights=stocGradAscent1(trainingSet,trainingLabels,500)
    #testSet=[]
    testLabel=[]
    errorSum=0
    for line in frTest.readlines():
        currentline=line.strip().split('\t')
        lineArr=[]
        for i in range(21):
            lineArr.append(float(currentline[i]))
        testdata=array(lineArr)
        h=classifyVector(testdata,weights)
        testLabel.append(float(currentline[21]))
        if int(h)!=int(currentline[21]):
            errorSum+=1

    errorRate=float(errorSum)/len(testLabel)

    print ("the error rate of this test is: %f" % errorRate)
    return errorRate


def multiTest(numTests=10):
    errorSum=0
    for i in range(numTests):
        errorSum+=colicTest()

    print ("after %d iterations the average error rate is: %f" % (numTests, errorSum/float(numTests)))


if __name__=='__main__':
    dataM,labelM=loadDataSet()
    #weight=gradAscent(dataM,labelM)
    #plotBestFit(weight)
    #weights=stocGradAscent1(dataM,labelM)
    #plotBestFit(weights)
    #print(dataM[:10])
    multiTest(20)
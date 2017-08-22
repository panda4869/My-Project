from numpy import *


def loadSimpData():
    datMat = matrix([[ 1. ,  2.1],
        [ 2. ,  1.1],
        [ 1.3,  1. ],
        [ 1. ,  1. ],
        [ 2. ,  1. ]])
    classLabels = [1.0, 1.0, -1.0, -1.0, 1.0]
    return datMat,classLabels
def stumpClassify(dataMatrix,dimen,threshVal,threshIneq):
    retArray=ones((dataMatrix.shape[0],1))
    if threshIneq=='lt':
        retArray[dataMatrix[:,dimen]<=threshVal]=-1.0
    else:
        retArray[dataMatrix[:,dimen]>threshVal]=-1.0
    return retArray

def buildStump(dataArr,classLabels,D):
    dataMat=mat(dataArr);classMat=mat(classLabels).T
    minError=inf
    m,n=shape(dataMat)
    bestStump={};bestClasEst=mat(zeros((m,1)))
    numStep=10.0
    for i in range(n):
        rangeMin=dataMat[:,i].min(); rangeMax=dataMat[:,i].max()
        stepSize=(rangeMax-rangeMin)/numStep
        for j in range(-1,int(numStep)+1):
            for Ineq in ['lt','gt']:
                threshVal=rangeMin+float(j)*stepSize
                classVal=stumpClassify(dataMat,i,threshVal,Ineq)
                errorArr=mat(ones((m,1)))
                errorArr[classVal==classMat]=0
                weightedError=D.T*errorArr
                if weightedError<minError:
                    minError=weightedError
                    bestStump['dim']=i
                    bestStump['thresh']=threshVal
                    bestStump['ineq']=Ineq
                    bestClasEst=classVal.copy()

    return bestStump,minError,bestClasEst


def adaBoostTrainDS(dataArr,classLabels,numIt=40):
    weakClassArr=[]
    m=shape(dataArr)[0]
    aggClassEst=mat(zeros((m,1)))
    D=mat(ones((m,1)))/m
    for i in range(numIt):
        bestStump,error,classEst=buildStump(dataArr,classLabels,D)       
        #print('D: ',D.T)
        alpha=float(0.5*log((1-error)/max(error,1e-16)))
        #print('alpha: ',alpha)
        bestStump['alpha']=alpha
        weakClassArr.append(bestStump)
        expon=multiply(-1.0*alpha*mat(classLabels).T,classEst)
        D=multiply(D,exp(expon))
        D=D/D.sum()
        aggClassEst+=alpha*classEst
        #print('aggclassEst: ',aggClassEst.T)
        errorRate=multiply(sign(aggClassEst)!=mat(classLabels).T,ones((m,1))).sum()/m
        #print('The aggErrorRate: ', errorRate)
        if errorRate==0.0: break
    return weakClassArr,aggClassEst

def adaClassify(datToClass,classifierArr):
    dataMatrix=mat(datToClass)
    m=shape(dataMatrix)[0]
    aggclassEst=mat(zeros((m,1)))
    for i in range(len(classifierArr)):
        classEst=stumpClassify(dataMatrix,classifierArr[i]['dim'],classifierArr[i]['thresh'],classifierArr[i]['ineq'])
        aggclassEst+=classEst*classifierArr[i]['alpha']
    return sign(aggclassEst)


def loadDataSet(fileName): 
    numFeat=len(open(fileName,'r').readline().split('\t'))
    fr=open(fileName,'r')
    dataMat=[];labelMat=[]
    for line in fr.readlines():
        lineArr=[]
        currentL=line.strip().split('\t')
        for i in range(numFeat-1):
            lineArr.append(float(currentL[i]))
        dataMat.append(lineArr)
        labelMat.append(float(currentL[-1]))
    fr.close()
    return dataMat,labelMat



def plotROC(predStrengths, classLabels):
    import matplotlib.pyplot as plt
    cur=(1.0,1.0)
    ySum=0.0
    numPositive=sum(array(classLabels)==1.0)
    yStep=1/float(numPositive)
    xStep=1/float(len(classLabels)-numPositive)
    fig=plt.figure(figsize=(12,10))
    fig.clf()
    ax=plt.subplot(111)
    predStrengthIndex=predStrengths.argsort()
    for index in predStrengthIndex.tolist()[0]:

        if classLabels[index]==1.0:
            deltaX=0;deltaY=yStep
        else:
            deltaX=xStep;deltaY=0
            ySum+=cur[1]
        #print([cur[0],cur[0]-deltaX],[cur[1],cur[1]-deltaY])
        ax.plot([cur[0],cur[0]-deltaX],[cur[1],cur[1]-deltaY],c='b')
        cur=(cur[0]-deltaX,cur[1]-deltaY)
    ax.plot([0,1],[0,1],'b--')
    ax.axis([0,1,0,1])
    plt.xlabel('False Positive');plt.ylabel('True Positive')
    plt.title('ROC Curve')
    plt.show()
    print ("the Area Under the Curve is: ",ySum*xStep)

if __name__=='__main__':
    #dataM,classM=loadSimpData()
    #D=mat(ones((5,1)))/5
    #bestS,minE,bestC=buildStump(dataM,classM,D)
    #print(bestS,minE,bestC)
    #weakC,aggC=adaBoostTrainDS(dataM,classM,9)
    #print(adaClassify(dataM,weakC)==mat(classM).T)
    dataArr,labelArr=loadDataSet('horseColicTraining2.txt')
    weakClassA,aggC=adaBoostTrainDS(dataArr,labelArr,10)
    testArr,labeltest=loadDataSet('horseColicTest2.txt')
    prediction10=adaClassify(testArr,weakClassA)
    #print('Error Number: ',sum((prediction10!=mat(labeltest).T)))
    #print(aggC)
    plotROC(aggC.T,labelArr)
    
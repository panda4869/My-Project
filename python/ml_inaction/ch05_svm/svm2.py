from numpy import *


def loadDataSet(fileName):
    dataMat=[]
    labelMat=[]
    fr=open(fileName,'r')
    for line in fr.readlines():
        currentline=line.strip().split('\t')
        dataMat.append([float(currentline[0]),float(currentline[1])])
        labelMat.append(float(currentline[2]))
    fr.close()
    return dataMat,labelMat

def selectJrand(i,m):
    import random
    j=i
    while (i==j):
        j=random.uniform(0,m)


    return j

def clipAlpha(aj,H,L):
    if aj>H:
        aj=H
    if aj<L:
        aj=L
    return aj

def smoSimple(dataMatIn, classLabels, C, toler, maxIter):
    dataMatIn=mat(dataMatIn)
    m,n=shape(dataMatIn)
    classLabels=mat(classLabels).transpose()
    alphas=mat(zeros((m,1)))
    b=0
    iter=0
    while (iter<=maxIter):
        num_changed_alphas=0
        for i in range(m):
            fi=float(multiply(alphas,classLabels).T*(dataMatIn*dataMatIn[i,:].T)+b)
            Ei=fi-float(classLabels[i])
            if ((classLabels[i]*Ei<-toler) & (alphas[i]<C)) | ((classLabels[i]*Ei>toler) & (alphas[i]>0)):

                j=selectJrand(i,m)
                fj=float(multiply(alphas,classLabels).T*(dataMatIn*dataMatIn[j,:].T)+b)
                Ej=fj-float(classLabels[j])
                alphas_oldi=alphas[i].copy()
                alphas_oldj=alphas[j].copy()
                if classLabels[i]!=classLabels[j]:
                    L=max(0,alphas[j]-alphas[i]); H=min(C,C+alphas[j]-alphas[i])
                else:
                    L=max(0,alphas[i]+alphas[j]-C); H=min(C,alphas[i]+alphas[j])
                if L==H: continue
                eta=2.0*dataMatIn[i,:]*dataMatIn[j,:].T-dataMatIn[i,:]*dataMatIn[i,:].T-dataMatIn[j,:]*dataMatIn[j,:].T
                if eta>=0: continue
                alphas[j]-=classLabels[j]*(Ei-Ej)/eta
                alphas[j]=clipAlpha(alphas[j],H,L)
                if (abs(alphas[j]-alphas_oldj)<0.00001): continue #print('J not move enough') 
                    
                alphas[i]+=classLabels[i]*classLabels[j]*(alphas_oldj-alphas[j])
                b1=b-Ei- classLabels[i]*(alphas[i]-alphas_oldi)*(dataMatIn[i,:]*dataMatIn[i,:].T)-classLabels[j]*(alphas[j]-alphas_oldj)*(dataMatIn[i,:]*dataMatIn[j,:].T)
                b2=b-Ej-classLabels[i]*(alphas[i]-alphas_oldi)*(dataMatIn[i,:]*dataMatIn[j,:].T)-classLabels[j]*(alphas[j]-alphas_oldj)*(dataMatIn[j,:]*dataMatIn[j,:].T)
                if 0<alphas[i]<C: b=b1
                elif 0<alphas[j]<C : b=b2
                else: b=(b1+b2)/2.0
                num_changed_alphas+=1
                #print ('iter: %d,i:%d,%d of pairs changed'%(iter,i,num_changed_alphas))
        if num_changed_alphas==0:
            iter+=1
        else:
            iter=0
        #print('Iteration number is %d'%iter)
    return b,alphas


def kernelTrans(X, A, kTup):
    m,n = shape(X)
    k = mat(zeros((m,1)))
    if kTup[0]=='lin': k = X * A.T
    elif kTup[0]=='rbf':
        for j in range(m):
            deltarow = X[j,:] - A
            k[j]=deltarow * deltarow.T
        k = exp(k/ (-1 * kTup[1]**2))
    else: raise NameError ('Kernal Name is not Right')

    return k 

class optStructK:
    def __init__(self,dataMatIn,classLabels,toler,C,kTup):
        self.X=dataMatIn
        self.m=shape(dataMatIn)[0]
        self.labelMat=classLabels
        self.toler=toler
        self.b=0
        self.C=C
        self.alphas=mat(zeros((self.m,1)))
        self.eCache=mat(zeros((self.m,2)))
        self.K=mat(zeros((self.m,self.m)))
        for i in range(self.m):
            self.K[:,i]=kernelTrans(self.X,self.X[i,:],kTup)

def calcEkK(oS, k):
    #print(oS.X)
    fi=float(multiply(oS.alphas,oS.labelMat).T*oS.K[:,k])+oS.b
    Ek=fi-float(oS.labelMat[k])
    return Ek

def selectJK(i, oS, Ei): 
    deltaEk=0;maxK=0;maxdelta=-1
    oS.eCache[i]=[1,Ei]
    validecachelist=nonzero(oS.eCache[:,0].A)[0]
    if len(validecachelist)>1:
        for k in validecachelist:
            if k==i: continue
            Ek=calcEkK(oS,k)
            deltaEk=abs(Ek-Ei)
            if (deltaEk>maxdelta):
                maxk=k;maxdelta=deltaEk;Ej=Ek
        return maxk,Ej
    else:
        j=selectJrand(i,oS.m)
        Ej=calcEkK(oS,j)
    return j , Ej

def updateEkK(oS, k):
    Ek=calcEkK(oS,k)
    oS.eCache[k]=[1,Ek]

def innerL(i,oS):
    Ei=calcEkK(oS,i)
    if ((oS.labelMat[i]*Ei<-oS.toler) & (oS.alphas[i]<oS.C)) | ((oS.labelMat[i]*Ei>oS.toler) & (oS.alphas[i]>0)):
        j,Ej=selectJK(i,oS,Ei)
        alphas_oldi=oS.alphas[i].copy(); alphas_oldj=oS.alphas[j].copy()
        if oS.labelMat[i]!=oS.labelMat[j]:
            L=max(0,oS.alphas[j]-oS.alphas[i]); H=min(oS.C,oS.C+oS.alphas[j]-oS.alphas[i])
        else:
            L=max(0,oS.alphas[i]+oS.alphas[j]-oS.C); H=min(oS.C,oS.alphas[i]+oS.alphas[j])
        if L==H: return 0
        eta=2.0 * oS.K[i,j]-oS.K[i,i].T-oS.K[j,j]
        if eta>=0: return 0
        oS.alphas[j]-=oS.labelMat[j]*(Ei-Ej)/eta
        oS.alphas[j]=clipAlpha(oS.alphas[j],H,L)
        updateEkK(oS,j)
        if abs(oS.alphas[j]-alphas_oldj)<0.00001: return 0
        oS.alphas[i]+=oS.labelMat[i]*oS.labelMat[j]*(alphas_oldj-oS.alphas[j])
        updateEkK(oS,i)
        b1=oS.b-Ei-oS.labelMat[i]*(oS.alphas[i]-alphas_oldi)*oS.K[i,i]-oS.labelMat[j]*(oS.alphas[j]-alphas_oldj)*oS.K[i,j]
        b2=oS.b-Ej-oS.labelMat[i]*(oS.alphas[i]-alphas_oldi)*oS.K[i,j]-oS.labelMat[j]*(oS.alphas[j]-alphas_oldj)*oS.K[j,j]
        if 0<oS.alphas[i]<oS.C: oS.b=b1
        elif 0<oS.alphas[j]<oS.C: oS.b=b2
        else: oS.b=(b1+b2)/2.0 
        return 1 
    else:
        return 0

def smoPK(dataMatIn, classLabels, C, toler, maxIter,kTup=('lin',0)): 
    oS=optStructK(mat(dataMatIn),mat(classLabels).transpose(),toler,C,kTup)
    entireset=True
    iter=0
    num_changed_alphas=0
    while (iter<maxIter) & ((num_changed_alphas>0) | (entireset)):
        num_changed_alphas=0
        if entireset:
            for i in range(oS.m):
                num_changed_alphas+=innerL(i,oS)
                print('fullset, iter: %d, i: %d, pairs: %d'%(iter,i,num_changed_alphas))
            iter+=1
        else:
            numvalidlist=nonzero((oS.alphas.A>0)*(oS.alphas.A<C))[0]
            for i in numvalidlist:
                num_changed_alphas+=innerL(i,oS)
                print('non-bound, iter: %d, i: %d, pairs: %d'%(iter,i,num_changed_alphas))
            iter+=1
        #print(num_changed_alphas)
        if entireset: entireset=False
        elif (num_changed_alphas==0): entireset=True
        print('iteration number: %d'%iter)
    return oS.b,oS.alphas

def calcWs(alphas,dataArr,classLabels):
    X=mat(dataArr); labels=mat(classLabels).transpose()
    m,n=shape(dataArr)
    w=zeros((n,1))
    for i in range(m):
        w+=multiply(alphas[i]*labels[i],X[i,:].T)
    return w

def testRbf(k1=1.3):
    dataArr,labelArr = loadDataSet('testSetRBF.txt')

    b,alphas=smoPK(dataArr,labelArr,200,0.0001,10000,('rbf',k1))
    dataMat=mat(dataArr); labelMat=mat(labelArr).transpose()
    m,n=shape(dataMat)
    sVsInd=nonzero(alphas.A>0)[0]
    sVs=dataMat[sVsInd]

    print ("there are %d Support Vectors" % shape(sVs)[0])

    errorCount=0
    for i in range(m):
        kernalEval=kernelTrans(sVs,dataMat[i,:],('rbf',k1))
        predict= kernalEval.T*multiply(alphas[sVsInd],labelMat[sVsInd])+b
        if sign(predict)!=sign(labelMat[i]): errorCount+=1

    print ("the training error rate is: %f" % (float(errorCount)/m))


    dataArr1,labelArr1 = loadDataSet('testSetRBF2.txt')
    dataMat1=mat(dataArr1); labelMat1=mat(labelArr1).transpose()
    m,n=shape(dataMat1)
    errorCount1=0
    for i in range(m):
        kernalEval1=kernelTrans(sVs,dataMat1[i,:],('rbf',k1))
        predict= kernalEval1.T*multiply(alphas[sVsInd],labelMat[sVsInd])+b
        if sign(predict)!=sign(labelMat1[i]): errorCount1+=1


    print ("the test error rate is: %f" % (float(errorCount1)/m))



def img2vector(filename):
    returnVect=zeros((1,1024))
    fr=open(filename,'r')
    for i in range(32):
        line=fr.readline()
        for j in range(32):
            returnVect[0,32*i+j]=int(line[j])
    fr.close()
    return returnVect

def loadImages(dirName):
    from os import listdir
    filenamelist=[f for f in listdir(dirName) if f.endswith('.txt')]
    m=len(filenamelist)
    trainingMat=zeros((m,1024))
    hwLabels=[]
    for i in range(m):
        filename=filenamelist[i]
        #print(filename)
        filestr=filename.split('.')[0]

        classlabel=int(filestr.split('_')[0])
        if classlabel==9: hwLabels.append(-1)
        else: hwLabels.append(1)
        trainingMat[i,:]=img2vector('%s/%s'%(dirName,filename))

    return trainingMat,hwLabels

def testDigits(kTup=('rbf', 10)):
    dataArr,labelArr=loadImages('trainingDigits')
    b,alphas=smoPK(dataArr,labelArr,200,0.0001,10000,kTup)
    dataMat=mat(dataArr); labelMat=mat(labelArr).transpose()
    m,n=shape(dataMat)
    sVsind=nonzero(alphas.A>0)[0]
    sVs=dataMat[sVsind]
    print ("there are %d Support Vectors" % shape(sVs)[0])
    labelsVs=labelMat[sVsind]
    errorCount=0
    for i in range(m):
        kernalEval=kernelTrans(sVs,dataMat[i,:],kTup)
        predict=kernalEval.T*multiply(labelsVs,alphas[sVsind])+b
        if sign(predict)!=sign(labelMat[i]): errorCount+=1
    print ("the training error rate is: %f" % (float(errorCount)/m))
    dataArr1,labelArr1=loadImages('testDigits')
    dataMat1=mat(dataArr1); labelMat1=mat(labelArr1).transpose()
    m,n = shape(dataMat1)
    errorCount1=0
    for i in range(m):
        kernalEval1=kernelTrans(sVs,dataMat1[i,:],kTup)
        predict=kernalEval1.T*multiply(labelsVs,alphas[sVsind])+b
        if sign(predict)!=sign(labelMat1[i]): errorCount1+=1

    print ("the test error rate is: %f" % (float(errorCount1)/m)) 


if __name__=='__main__':
    #dataM,labelM=loadDataSet('testSet.txt')
    #b,alphas=smoSimple(dataM,labelM,0.6,0.001,40)
    #print(b,alphas[alphas>0])
    #b,alphas=smoPK(dataM,labelM,0.6,0.001,40)
    #w=calcWs(alphas,dataM,labelM)
    #print(mat(dataM[0])*mat(w)+b)
    testDigits(('rbf',20))

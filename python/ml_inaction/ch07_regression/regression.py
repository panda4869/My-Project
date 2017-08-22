from numpy import *
import matplotlib.pyplot as plt

def loadDataSet(fileName):
    numFeat=len(open(fileName,'r').readline().strip().split('\t'))-1
    fr=open(fileName,'r')
    dataMat=[]
    labelMat=[]
    for line in fr.readlines():
        lineArr=[]
        curline=line.strip().split('\t')
        for i in range(numFeat):
            lineArr.append(float(curline[i]))
        dataMat.append(lineArr)
        labelMat.append(float(curline[-1]))
    return dataMat,labelMat

def standRegres(xArr,yArr):
    xMat=mat(xArr);yMat=mat(yArr).T
    m,n=shape(xMat)
    #xTx=mat(zeros((m,m)))
    xTx=xMat.T*xMat
    if linalg.det(xTx)==0:
        print('The determinant is zero. Can not inverse')
        return 0

    ws=xTx.I*(xMat.T*yMat)
    return  ws


def lwlr(testPoint,xArr,yArr,k=1.0):
    xMat=mat(xArr);yMat=mat(yArr).T
    testPointMat=mat(testPoint)
    m,n=shape(xMat)
    weights=mat(eye(m))
    for i in range(m):
        dist=testPointMat-xMat[i,:]
        #print(-dist*dist.T)
        weights[i,i]=exp(-dist*dist.T/(2.0*k**2))
    xTx=xMat.T*(weights*xMat)
    if linalg.det(xTx)==0:
        print('The determinant is zero. Can not inverse')
        return 0
    #print(weights*yMat))
    ws=xTx.I*(xMat.T*(weights*yMat))
    return testPoint*ws
def lwlrTest(testArr,xArr,yArr,k=1.0):
    xMat=mat(xArr);yMat=mat(yArr).T
    m,n=shape(xMat)
    yHat=zeros(m)
    t=len(testArr)
    for i in range(t):
        yHat[i]=lwlr(testArr[i],xArr,yArr,k)

    return yHat

def rssError(yArr,yHatArr):
    return sum((yArr-yHatArr)**2)

def ridgeRegress(xMat,yMat,lam=0.2):
    m,n=shape(xMat)
    denom=xMat.T*xMat+lam*mat(eye(n))
    if linalg.det(denom)==0:
        print ('Can not inverse')
        return 0
    #print(shape(xMat.T*yMat))
    ws=denom.I*(xMat.T*yMat)
    return ws

def ridgeTest(xArr,yArr):
    xMat=mat(xArr);yMat=mat(yArr).T
    numberTest=30
    wMat=mat(zeros((numberTest,shape(xMat)[1])))
    center=mean(xMat,0)
    scale=var(xMat,0)
    xMat_Scaled=(xMat-center)/scale
    #print(shape(xMat_Scaled))
    for i in range(numberTest):
        wMat[i,:]=ridgeRegress(xMat_Scaled,yMat,exp(i-10)).T

    return wMat


def regularize(xMat):
    inMat=xMat.copy()
    meanMat=mean(inMat,0)
    varMat=var(inMat,0)
    inMat=(inMat-meanMat)/varMat
    return inMat

def stageWise(xArr,yArr,eps=0.01,numIt=100):
    xMat=mat(xArr);yMat=mat(yArr).T
    #yMat=yMat-mean(yMat,0)

    xMat=regularize(xMat)
    m,n=shape(xMat)
    #print(xMat)
    ws=zeros((n,1));wsMax=ws.copy();wsTest=ws.copy()
    for i in range(numIt):
        print(ws.T)
        lowestError=inf;
        for j in range(n):
            for sign in [-1,1]:
                wsTest=ws.copy()
                wsTest[j]+= eps*sign
                yTest=xMat*wsTest
                rssE=rssError(yMat.A,yTest.A)
                if rssE<lowestError:                  
                    lowestError=rssE
                    wsMax=wsTest

        ws = wsMax.copy()


#AIzaSyDVKiiQ3IhC4Grop5ZOjSgkEW_sixjcppI
def scrapePage(retX,retY,setNumber,yr,numPce,origPrc):
    import os
    from bs4 import BeautifulSoup
    path=os.getcwd()+'/setHtml'
    for file in os.listdir(path):
        if file.find(str(setNumber))!=-1:
            fr=open(path+'/'+file,'r')
            soup=BeautifulSoup(fr.read(),'html.parser')
            i=1
            currentRow=soup.find_all('table',r='%d'%i)
            #print(currentRow[0].find_all('a')[1].contents[0])
            while(len(currentRow)!=0):
                title=currentRow[0].find_all('a')[1].contents[0]
                tlwc=title.lower()
                if (tlwc.find('new')!=-1 or (tlwc.find('nisb')!=-1)):
                    newflag=1
                else:
                    newflag=0
                if ((currentRow[0].find_all('td','prc bidsold g-b')!=-1) or (currentRow[0].find_all('div','bidsold g-b')[0].text!=-1)):
                    sellprice=currentRow[0].find_all('td')[4].text
                    sellprice=sellprice.replace('$','')
                    sellprice=sellprice.replace(',','')
                    sellprice=sellprice.replace('Free shipping','')
                    try:
                        sellprice=float(sellprice)
                        if sellprice> 0.5 * origPrc:
                            retX.append([yr,numPce,newflag,origPrc])
                            retY.append(sellprice)
                            #print ('%d %d %d %d %f'%(yr,numPce,newflag,origPrc,sellprice))
                    except:
                        pass
                i+=1
                currentRow=soup.find_all('table',r='%d'%i)

               


def scrapePage1(retX,retY,setNumber,yr,numPce,origPrc):
    import os
    from bs4 import BeautifulSoup
    path=os.getcwd()+'/setHtml'
    for file in os.listdir(path):
        if file.find(str(setNumber))!=-1:
            fr=open(path+'/'+file,'r')
            soup=BeautifulSoup(fr.read(),'html.parser')
            i=4
            currentRow=soup.find_all('table',r='%d'%i)
            print(currentRow[0].find_all('td')[4].text)
            #try:
                #print(currentRow[0].find_all('td','prc bidsold g-b')[0].text.strip())
            #except:
                #print(currentRow[0].find_all('div','bidsold g-b')[0].text.strip())



def setDateCollect(retX,retY):
    scrapePage(retX, retY, 8288, 2006, 800, 49.99)
    scrapePage(retX, retY, 10030, 2002, 3096, 269.99)
    scrapePage(retX, retY, 10179, 2007, 5195, 499.99)
    scrapePage(retX, retY, 10181, 2007, 3428, 199.99)
    scrapePage(retX, retY, 10189, 2008, 5922, 299.99)
    scrapePage(retX, retY, 10196, 2009, 3263, 249.99)


def crossvalidation(xArr,yArr,numVal=10):
    m=len(xArr)
    indexlist=list(range(m))
    ErrorMat=zeros((numVal,30))
    for i in range(numVal):
        x_train=[];y_train=[]
        x_test=[];y_test=[]
        random.shuffle(indexlist)
        for j in range(m):
            if j < 0.9*m:
                x_train.append(xArr[indexlist[j]])
                y_train.append(yArr[indexlist[j]])
            else:
                x_test.append(xArr[indexlist[j]])
                y_test.append(yArr[indexlist[j]])
        #print(shape(x_train))
        wMat=ridgeTest(x_train,y_train)
        for k in range(30):
            x_trainmat=mat(x_train);x_testmat=mat(x_test)
            mean_train=mean(x_trainmat,0)
            var_train=var(x_trainmat,0)
            x_test_scale=(x_testmat-mean_train)/var_train
            yHat=x_test_scale*wMat[k,:].T
            ErrorMat[i,k]=rssError(yHat.T.A,mat(y_test).A)
    meanError=mean(ErrorMat,0)
    #print(meanError.A)
    varError=var(ErrorMat,0)
    minError=float(min(meanError))
    wMat=ridgeTest(xArr,yArr)
    bestWeights=wMat[nonzero(minError==meanError)]
    xMat=mat(xArr);yMat=mat(yArr).T
    unreg=bestWeights/var(xMat,0)
    constant=-1.0*sum(multiply(mean(xMat,0),unreg))
    print('the best model from Ridge Regree is :\n ',unreg)
    print('with constant term:',constant)



if __name__=='__main__':
    xArr1,yArr1=loadDataSet('abalone.txt')
    #yHat=lwlrTest(xArr1[0:99],xArr1[0:99],yArr1[0:99],k=0.1)
    #xMat1=mat(xArr1)
    #yMat1=mat(yArr1).T
    #index=xMat1[:,1].argsort(0)
    #xSort=xMat1[index][:,0,:]
    #plt.figure()
    #ax=plt.subplot(111)
    #ax.scatter(xMat1[:,1].flatten().A[0],yMat1.flatten().A[0],c='b')
    #ax.plot(xSort[:,1].flatten().A[0],yHat[index],'-')
    #plt.show()
    #print(rssError(yArr1[0:99],yHat.T))
    #ridgeweights=ridgeTest(xArr1,yArr1)
    #plt.figure()
    #ax=plt.subplot(111)
    #ax.plot(ridgeweights)
    #plt.show()
    #print(regularize(mat(xArr1)))
    #print(stageWise(xArr1,yArr1,0.001,5000))
    retX1=[]
    retY1=[]
    setDateCollect(retX1,retY1)
    m,n=shape(retX1)
    lgx1=mat(ones((m,5)))
    lgx1[:,1:5]=mat(retX1)
    ws=standRegres(lgx1,retY1)
    #print(lgx1[-1]*ws)
    crossvalidation(retX1,retY1,10)
    print(ws)


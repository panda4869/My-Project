from numpy import *
import matplotlib.pyplot as plt

def loadDataSet(fileName):
    import os
    dataMat=[]
    path=os.getcwd()+'/'+fileName
    fr=open(path,'r')
    for line in fr.readlines():
        currentline=line.strip().split('\t')
        dataMat.append(list(map(float,currentline)))
    return mat(dataMat)
def distEclud(vecA,vecB):
    return sqrt(sum(power(vecA-vecB,2)))

def randCent(dataSet,k):
    n=shape(dataSet)[1]
    centroids=mat(zeros((k,n)))
    for j in range(n):
        minJ=min(dataSet[:,j])
        rangeJ=float(max(dataSet[:,j])-minJ)
        centroids[:,j]=minJ+rangeJ*random.rand(k,1)
    return centroids

def kMeans(dataSet,k,distMeans=distEclud,createCent=randCent):
    m,n=shape(dataSet)
    clusterAssemnt=mat(zeros((m,2)))
    centroids=createCent(dataSet,k)
    clusterchange=True
    while clusterchange==True:
        clusterchange=False
        for i in range(m):
            minIndex=-1
            mindist=inf
            for j in range(k):
                dist=distMeans(dataSet[i,:],centroids[j,:])
                if dist<mindist:
                    mindist=dist
                    minIndex=j
            if clusterAssemnt[i,0]!=minIndex: clusterchange=True
            clusterAssemnt[i,:]=minIndex,mindist**2
        #print(centroids)
        for i in range(k):
            ptsInclust=dataSet[nonzero(clusterAssemnt[:,0].A==i)[0]]
            centroids[i,:]=mean(ptsInclust,axis=0)
    return centroids,clusterAssemnt

def biKmeans(dataset,k,distMeans=distEclud):
    m=shape(dataset)[0]
    clusterAssemnt=mat(zeros((m,2)))
    centroids0=mean(dataset,axis=0).tolist()[0]
    centList=[centroids0]
    for i in range(m):
        clusterAssemnt[i,1]=distMeans(mat(centroids0),dataset[i,:])**2
    while len(centList)<k:
        minSSE=inf
        for i in range(len(centList)):
            pointtosplit=dataset[nonzero(clusterAssemnt[:,0].A==i)[0],:]
            splitcent,splitAss=kMeans(pointtosplit,2,distMeans)
            splitSSE=sum(splitAss[:,1])
            nosplitSSE=sum(clusterAssemnt[nonzero(clusterAssemnt[:,0].A!=i)[0],1])
            #print('split: ',splitSSE,'nonsplit: ',nosplitSSE)
            #print('Sum',(splitSSE+nosplitSSE))
            #print(minSSE)
            if (splitSSE+nosplitSSE)<minSSE:
                bestTosplit=i
                bestsplitAss=splitAss.copy()
                minSSE=splitSSE+nosplitSSE
                bestsplitcent=splitcent
        bestsplitAss[nonzero(bestsplitAss[:,0].A==1)[0],0]=len(centList)
        bestsplitAss[nonzero(bestsplitAss[:,0].A==0)[0],0]=bestTosplit
        print('the bestsplit is :',bestTosplit)
        print('the len of bestclusAss is:', len(bestsplitAss))
        centList[bestTosplit]=bestsplitcent[0,:].tolist()[0]
        centList.append(bestsplitcent[1,:].tolist()[0])
        clusterAssemnt[nonzero(clusterAssemnt[:,0].A==bestTosplit)[0],:]=bestsplitAss

    return mat(centList),clusterAssemnt

def distSLC(VecA,VecB):
    a=sin(VecA[0,1]*pi/180)*sin(VecB[0,1]*pi/180)
    b=cos(VecA[0,1]*pi/180)*cos(VecB[0,1]*pi/180)*cos(pi*(VecB[0,0]-VecA[0,0])/180)
    return arccos(a+b)*6471.0

def clusterClubs(numbClust=5):
    datalist=[]
    fr=open('places.txt','r')
    for line in fr.readlines():
        curline=line.strip().split('\t')
        datalist.append([float(curline[3]),float(curline[4])])
    dataMat=mat(datalist)
    mycentroids,clusterAss=biKmeans(dataMat,numbClust,distMeans=distSLC)
    fig=plt.figure()
    approg=dict(xticks=[],yticks=[])
    rect=[0.1,0.1,0.8,0.8]
    ax0=fig.add_axes(rect,label='ax0',**approg)
    img=plt.imread('portland.png')
    ax0.imshow(img)
    makerlist=['s','o','^','8','p','d','v','h','>','<']
    ax1=fig.add_axes(rect,label='ax1',frameon=False)
    for i in range(numbClust):
        pltInCluster=dataMat[nonzero(clusterAss[:,0]==i)[0],:]
        ax1.scatter(pltInCluster[:,0].flatten().A[0],pltInCluster[:,1].flatten().A[0],marker=makerlist[i%len(makerlist)],s=90)
    ax1.scatter(mycentroids[:,0].flatten().A[0],mycentroids[:,1].flatten().A[0],marker='+',color='black',s=300)
    plt.show()



if __name__=='__main__':
    #dataMat3=loadDataSet('testSet2.txt')
    #print(randCent(dataMat1,2))
    #centList1,myNewAssesment=biKmeans(dataMat3,3)
    #print(centList1)
    #app=dict(xticks=[],yticks=[])
    #fig=plt.figure()
    #ax0=fig.add_axes([0.1,0.1,0.8,0.8],label='ax0',**app)
    #img=plt.imread('portland.png')
    #ax0.imshow(img)
    #plt.show()
    clusterClubs()


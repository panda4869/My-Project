from numpy import *

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




if __name__=='__main__':
    dataMat3=loadDataSet('testSet2.txt')
    #print(randCent(dataMat1,2))
    centList1,myNewAssesment=biKmeans(dataMat3,3)
    print(centList1)


from numpy import *

def loadDataSet():
    return [[1, 3, 4], [2, 3, 5], [1, 2, 3, 5], [2, 5]]

def createC1(dataSet):
    c1=[]
    for transaction in dataSet:
        for item in transaction:
            if [item] in c1:
                continue
            else:
                c1.append([item])
    c1.sort()
    return list(map(frozenset,c1))

def scanD(D,Ck,minSupport):
    scan={}
    for transaction in D:
        for c in Ck:
            
            #    print(transaction)
            if c.issubset(transaction):
                if not c  in scan:
                    scan[c]=1
                else: 
                    scan[c]+=1
    num_can=float(len(D))
    #print(num_can)
    #print(scan)
    retList=[]
    supportData={}
    for key in scan:
        support=scan[key]/num_can 
        if support>=minSupport:
            retList.insert(0,key)
        supportData[key]=support


    return retList,supportData

def aprioriGen(Lk,k):
    retList=[]
    listLen=len(Lk)
    for i in range(listLen):
        for j in range(i+1,listLen):
            L1=list(Lk[i])[:k-2];L2=list(Lk[j])[:k-2]
            L1.sort();L2.sort()
            if L1==L2:
                retList.append(Lk[i] | Lk[j])
    return retList

def apriori(dataSet,minSupport):
    data=list(map(set,dataSet))
    Ck=createC1(data)
    L1,supportData=scanD(data,Ck,minSupport)
    L=[L1]
    k=2
    while(len(L[k-2])>0):
        retl=aprioriGen(L[k-2],k)
        lk,support=scanD(data,retl,minSupport)
        L.append(lk)
        supportData.update(support)
        k+=1

    return L,supportData


if __name__=='__main__':

    dat=loadDataSet()
    #print(dat)
    #dat=list(map(set,dat))
    #print(dat)
    #cc1=createC1(dat)
    #dat=list(map(set,dat))
    #L1,supportD=scanD(dat,cc1,0.5)
    #print(aprioriGen(L1,2))
    l,supportD=apriori(dat,0.5)
    print(l)
    print(supportD)
    #for i in cc1:
    #    print(i)
    #print(cc1)
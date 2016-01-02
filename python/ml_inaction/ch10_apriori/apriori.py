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

            print(transaction)
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




if __name__=='__main__':

    dat=loadDataSet()
    #print(dat)
    #dat=list(map(set,dat))
    #print(dat)
    cc1=createC1(dat)

    L1,supportD=scanD(dat,cc1,0.5)
    #print(L1)

    #for i in cc1:
    #    print(i)
    #print(cc1)
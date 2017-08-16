from numpy import *

def loadDataSet(fileName):
    numFeat=len(open(fileName,'r').readline().strip().split('\t'))-1
    fr=open(fileName,'r')
    dataMat=[]
    labelMat=[]
    for line in fr.readlines():
        lineArr=[]
        curline=line.strip().split('\t')
        for i in range(numFeat):
            lineArr.append(curline[i])
        dataMat.append(lineArr)
        labelMat.append(curline[-1])
return dataMat,labelMat



if __name__=='__main__':
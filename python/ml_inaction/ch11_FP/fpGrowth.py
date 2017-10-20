from numpy import *


class treeNode:
    def __init__(self,namValue,nameOccur,parentNode):
        self.name=namValue
        self.count=nameOccur
        self.parent=parentNode
        self.nodeLink=None
        self.children={}
    def inc(self,nameOccur):
        self.count+=nameOccur
    def disp(self,ind=1):
        print('  '*ind,self.name,' ',self.count)
        for child in self.children.values():
            child.disp(ind+1)

def createTree1(dataSet,minSup=1):
    headerTable={}
    for trans in dataSet:
        for item in trans:
            headerTable[item]=headerTable.get(item,0)+dataSet[trans]
    
    tablekey=list(headerTable.keys())
    #print(tablekey)
    for item in tablekey:
        if headerTable[item]<minSup:
            del headerTable[item]
    frequentItem=set(headerTable.keys())
    #print(headerTable)
    if len(frequentItem)==0: return None,None
    for item in frequentItem:
            headerTable[item]=[headerTable[item],None]

    retTree=treeNode('Null set',1,None)
    
    for trans, count in dataSet.items():
        localD={}
        for item in trans:
            if item in frequentItem:
                localD[item]=headerTable[item][0]
        if len(localD)>0:
                ordereditem=[v[0] for v in sorted(localD.items(),key= lambda x: x[1],reverse=True)]
                updateTree(ordereditem,retTree,headerTable,count)

    return retTree,headerTable

def createTree(dataSet, minSup=1): #create FP-tree from dataset but don't mine
    headerTable = {}
    #go over dataSet twice
    for trans in dataSet:#first pass counts frequency of occurance
        for item in trans:
            headerTable[item] = headerTable.get(item, 0) + dataSet[trans]
    tablekey=list(headerTable.keys())
    for k in tablekey:  #remove items not meeting minSup
        if headerTable[k] < minSup: 
            del(headerTable[k])
    freqItemSet = set(headerTable.keys())
    #print 'freqItemSet: ',freqItemSet
    if len(freqItemSet) == 0: return None, None  #if no items meet min support -->get out
    for k in headerTable:
        headerTable[k] = [headerTable[k], None] #reformat headerTable to use Node link 
    #print 'headerTable: ',headerTable
    retTree = treeNode('Null Set', 1, None) #create tree
    for tranSet, count in dataSet.items():  #go through dataset 2nd time
        localD = {}
        for item in tranSet:  #put transaction items in order
            if item in freqItemSet:
                localD[item] = headerTable[item][0]
        if len(localD) > 0:
            orderedItems = [v[0] for v in sorted(localD.items(), key=lambda p: p[1], reverse=True)]
            updateTree(orderedItems, retTree, headerTable, count)#populate tree with ordered freq itemset
    return retTree, headerTable #return tree and header table

def updateTree(item,inTree,headerTable,count):
    if item[0] in inTree.children:
        inTree.children[item[0]].inc(count)
    else:
        inTree.children[item[0]]=treeNode(item[0],count,inTree)
        if headerTable[item[0]][1]==None:
            headerTable[item[0]][1]=inTree.children[item[0]]
        else:
            updateHeader(headerTable[item[0]][1],inTree.children[item[0]])
    if len(item)>1:
        updateTree(item[1::],inTree.children[item[0]],headerTable,count)

def updateHeader(nodetoTest,targetNode):
    while (nodetoTest.nodeLink!=None):
        nodetoTest=nodetoTest.nodeLink
    nodetoTest.nodeLink=targetNode


def loadSimpDat():
    simpDat = [['r', 'z', 'h', 'j', 'p'],
               ['z', 'y', 'x', 'w', 'v', 'u', 't', 's'],
               ['z'],
               ['r', 'x', 'n', 'o', 's'],
               ['y', 'r', 'x', 'z', 'q', 't', 'p'],
               ['y', 'z', 'x', 'e', 'q', 's', 't', 'm']]
    return simpDat


def createInitSet(dataSet):
    retDict={}

    for trans in dataSet:
        retDict[frozenset(trans)]=1
    return retDict

def ascendTree(leafNode,prefixPath):
    if leafNode.parent!=None:
        prefixPath.append(leafNode.name)
        ascendTree(leafNode.parent,prefixPath)


def findPrefixPath(basePat,treenode):
    condPats={}
    
    while treenode!=None:
        prefixPath=[]
        ascendTree(treenode,prefixPath)
        if len(prefixPath)>1:
            condPats[frozenset(prefixPath[1:])]=treenode.count
        treenode=treenode.nodeLink
    return condPats

def mineTree(inTree,headerTable,minSup,prefix,frequentItem):
    bigL=[v[0] for v in sorted(headerTable.items(), key=lambda x: x[0][0])]
    for basePat in bigL:
        newfrequent=prefix.copy()
        newfrequent.add(basePat)
        frequentItem.append(newfrequent)
        condpattern=findPrefixPath(basePat,headerTable[basePat][1])
        mycondfp,myheader=createTree(condpattern,minSup)
        if myheader!=None:
            print('Conditional tree for: ',newfrequent)
            mycondfp.disp()
            mineTree(mycondfp,myheader,minSup,newfrequent,frequentItem)




if __name__=='__main__':
    #rootnode=treeNode('pyramid',9,None)
    #rootnode.children['eye']=treeNode('eye',13,None)
    #rootnode.children['phoenix']=treeNode('phoenix',3,None)
    #rootnode.disp()
    simpDat=loadSimpDat()
    initSet=createInitSet(simpDat)
    #print(initSet)
    myFPtree,myHeaderTab=createTree(initSet,3)
    #print([v[0] for v in sorted(myHeaderTab.items(),key=lambda x : x[0][0])])
    #print(myHeaderTab)
    #myFPtree.disp()
    #print(findPrefixPath('x',myHeaderTab['x'][1]))
    mineTree(myFPtree,myHeaderTab,3,set([]),[])

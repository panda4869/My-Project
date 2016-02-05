# -*- coding: utf-8 -*-
"""
Created on Tue Sep 22 17:35:24 2015

@author: panminxiang
"""
#Problem 1a


def decimals(n):
    num=1/n
    s=[]    
    l=str(num)
    def dec(l):
         s=0
         a=0
         while True:
            s=int(l[a+2])
            a+=1
            yield(s) 
        

    if(len(l)<18):
      for x in range(len(l)-2):
         s.append(int(l[x+2]))
         
      return(s)
    else:
       return(dec(l))
           

a=list(decimals(8))
a
d=decimals(3)     
[next(d) for x in range(10)]
#Problem 1b
def genlimit(d,n):
    if(type(d)!=list):
        return([next(d) for x in range(n)])
    elif(len(d)<n):
        return(d)
    else:
        return(d[:n])
        
print(list(genlimit(decimals(3), 5)))
print(list(genlimit(decimals(8), 5)))
#Problem 2
def decimals2(d):
     n=1
     #use a dict to store the sign for each mode
     s = {}
     v = n// d
     #calculate the mode
     n = 10 * (n - v * d)
     a = []
    
     if n == 0:
          return a
      
     while n > 0:
        
      prev = s.get(n, None)
      
      if prev!= None:
                start = prev
                non_r = a[:start]
                r = a[start:]
                return non_r + r+[r]
        
      #build the sign                
      s[n] = len(a)        
      v = n // d
      #append the result
      a.append(v)
     #compute mode again
      n -= v * d
      n *= 10
     return a

#test    
import textwrap

for j in range(2, 30): 
    # ugly hack needed because lines don't wrap in pdf version
    d = list(decimals2(j))
    print('   Expansion of 1/' + str(j) + ':')
    print( textwrap.fill(str(d), 80))
    
#Problem 3a
def select(I,selectors):
    a=list(I)
    s=[]
    for x in range(len(selectors)):
        if(type(selectors[x])!=str):
          if(selectors[x] == True):
              s.append(a[x])
        elif(len(selectors[x])!=0):
            s.append(a[x])
    
    return(s)
#test        
select(range(5), [0, 1, '', "foo", True])
#Problem 3b
def nbitIntToDigits(i,n):
     s=[]
     a= bin(i)[2:]
     if(len(a)<n):
        for i in range(n-len(a)): 
             s.append(0)
     for x in a:
        s.append(int(x))
     return(s)
 #test
[nbitIntToDigits(3, 2), nbitIntToDigits(3, 6), nbitIntToDigits(11, 4)]      
#Problem 3b  
from itertools import combinations    
combinations([1,2,3],2)
def powerSets(elements):
    if len(elements) > 0:
        head = elements[0]
        for tail in powerSets(elements[1:]):
            yield [head] + tail
            yield tail
    else:
        yield []
a=powerSets(['avery', 'math', 'butler']) 
#Problem 3c
def powerSet(s):
    def p(s):
      if len(s) <= 1:
         yield s
         yield []
      else:
         for x in p(s[1:]):
            yield [s[0]]+x
            yield x
    l=[]
    j=p(s)
    for a in range(2**len(s)):
        l.append(next(j))
    return(l)

powerSet(['avery', 'math', 'butler'])
powerSet(['avery', 'math', 'butler', 'dodge'])

#Problem 4
path = '/Users/panminxiang/Documents/text books/python/hw/hw/hw2/oil.txt'
o = open(path,'r')
o.readline()
def oil(path):
    i=0
    o=open(path,'r')
    data=o.readlines()
    b=0    
    while i<42:
       
        years=2014-i
        if(years%2==0):
             total=0
             m=[]
             n=0
             for x in data[(8+14*b):(21+14*b)]:
                 c=x.split(" ")
                 for a in range(c.count('')):
                      c.remove('')
                 if(n==0):
                       total=int(c[1].replace(',',''))
                 else:
                       m.append(float(c[5]))
                                                                  
                 n+=1
             i+=1
             print("%d: quan: total=%d prices: max=%f min=%f avg=%f"%(years,total,max(m),min(m),sum(m)/12))
             yield ''
             
        else:
             total=0
             m=[]
             n=0
             for x in data[(8+14*b):(21+14*b)]:
                 c=x.split(" ")
                 for a in range(c.count('')):
                      c.remove('')
                 if(n==0):
                       total=int(c[8].replace(',',''))
                 else:
                       m.append(float(c[12]))
                                                                  
                 n+=1
             if i==42:
                raise StopIteration  
             else:
                 i+=1
                 b+=1
             print("%d: quan: total=%d prices: max=%f min=%f avg=%f"%(years,total,max(m),min(m),sum(m)/12))
             yield ''
             
#test   
o=oil(path)
for s in list(o):
    print(s)


#Problem 5a
def countBases(dna):
    return [dna.count("A"),dna.count("C"),dna.count("G"),dna.count("T")]
#test
dna = 'CATCGATATCTCTGAGTGCAC'    
countBases('AC')    
countBases(dna)
#Problem 5b
def reverseComplement(dna):
    a=[]
    for x in dna:
        if(x=="A"):
            a.append("T")
        if(x=="C"):
            a.append("G")
        if(x=="G"):
            a.append("C")
        if(x=="T"):
            a.append("A")
    return("".join(reversed(a)))
    
reverseComplement(dna)



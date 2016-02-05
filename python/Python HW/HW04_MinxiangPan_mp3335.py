# -*- coding: utf-8 -*-
"""
Created on Fri Oct 16 14:26:12 2015

@author: panminxiang
"""
# Problem 1
def los(string):
    d={}
    for i in range(len(string)):
        s=string[i]
        
        for j in range(i+1,len(string)):
            
            if(max(s)<string[j] and j-len(s)==i):
                s+=string[j]
           
        d[s]=len(s)
    return(max(d, key=d.get))


los('abcd')
los('xabcyz')
los('cba')
los('larry')   
los('xabcxabcdexuvwxyz')
los('xabcxabcdexpuvwxyz')



# Problem 2
class Constraint():
    
    result=0
    v=[]
    r2=0
      
    def __init__(self,variable,coef,const):
        variable=variable.split(' ')
        self.v=variable
        self.variable={x:0 for x in variable}
        self.status={x:0 for x in variable}
        self.coef={x:0 for x in variable}
        for x in range(len(variable)):
            self.coef[variable[x]]=coef[x]
        self.c=const
    def setvar(self,variable,value):
        value=float(value)
        if isinstance(variable,int):
            variable=self.v[variable]
        
        
        self.variable[variable]=value
        self.status[variable]=1
        self.result+=self.variable[variable]*self.coef[variable] 
        
       
                 
        if  (list(self.status.values()).count(0)==1):
            r=[]
            for x in self.v:
                if(self.status[x]==0):
                    self.variable[x]=round((self.c-self.result)/self.coef[x],1)
                    self.status[x]=1
            for k,v in self.variable.items() :
                print("%s = %f"%(k,v))
                r.append(v)
            self.r2=self.result
            self.result=0
            self.status={x:0 for x in self.variable}
            return(r)
    def getstatus(self):
         return(self.status)
    def getresult(self):
         return(self.r2)
    def getvariable(self):
         return(self.variable)
    def getc(self):
         return(self.c)
    def getco(self):
        return(self.coef)
         
def c2f(temp):
     c = Constraint('C F', [9,-5], -5*32)
     c.setvar('C',temp)
     return(c.getvariable()['F'])

def f2c(temp):
     c = Constraint('C F', [9,-5], -5*32)
     c.setvar('F',temp)
     return(c.getvariable()['C'])
     
              
[c2f(0), c2f(100), f2c(32), f2c(212)]    

#Problem 3
from itertools import *
import functools
import operator       

def mindot(a,b):
    s=[] 
    for i in  permutations(a,len(a)):
      for j in permutations(b,len(b)):
        s.append(sum(map( operator.mul, i, j)))
    return(min(s))



v2a = [10,20]
v2b = [0, 1]

v3a = [1,3,-5]
v3b = [-2, 4, 1]

v4a = range(1,6)
v4b = [1,0,1,0,1] 

[mindot(v2a,v2b),mindot(v3a, v3b), mindot(v4a, v4b)]

#Problem 4
def pickitems(prices, cash):
        for x in range(len(prices)):
            for j in combinations(prices,x):
                if(sum(j)==cash):
                     return(j)
cash1 = 4
prices1= [1,1,1,1,8]

cash2 = 200
prices2 = [150, 24, 79, 50, 88, 345, 3]

cash3 = 8
prices3 = [2, 1, 9, 4, 4, 56, 90, 3]

cash4 = 542
prices4 = [230, 863, 916, 585, 981, 404, 316, 785, 
       88, 12, 70, 435, 384, 778, 887, 755, 740, 
       337, 86, 92, 325, 422, 815, 650, 920, 125,
       277, 336, 221, 847, 168, 23, 677, 61, 400,
       136, 874, 363, 394, 199, 863, 997, 794, 587,
       124, 321, 212, 957, 764, 173, 314, 422, 927,
       783, 930, 282, 306, 506, 44, 926, 691, 568,
       68, 730, 933, 737, 531, 180, 414, 751, 28, 
       546, 60, 371, 493, 370, 527, 387, 43, 541,
       13, 457, 328, 227, 652, 365, 430, 803, 59,
       858, 538, 427, 583, 368, 375, 173, 809, 896,
       370, 789]
[pickitems(prices1, cash1), pickitems(prices2, cash2), pickitems(prices3, cash3), pickitems(prices4, cash4)]       
 
#Problem 5
class secure(object):
     def __init__(self, f):
        # f is the original function
        self.f = f
     def __call__(self, user, pw, *pos, **kw):
         up = {}
         up['jack'] = 'jackpw'
         up['jill'] = 'jillpw'        
         if not user in up:
             raise Exception('User %s not registered' % user)
         if pw != up[user]:
            raise Exception("Bad password")
         return(self.f(*pos, **kw))
@secure 
def foo(a,b):
    return (a+b)
@secure    
def bar(a, b=34):
    return(a+b)
    
    
foo(1,2)
foo('jack', 'jackpw', 1 ,2)
foo('frank', 'bad', 1 ,2)
foo('jack', 'nope')
bar('jill', 'jillpw', 5, b=34)
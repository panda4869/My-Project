# -*- coding: utf-8 -*-
"""
Created on Mon Oct  5 19:29:34 2015

@author: panminxiang
"""
#problem 1a
import string

alphabet = string.ascii_lowercase
# Simple cipher

class swapaz:
    ''' swap 'a' and 'z' '''
    def encode(self, s):
        s = s.replace('a', 'A').replace('z', 'Z').replace('Z','a').replace('A', 'z')
        return(s)
        
    def decode(self, s):
        return(self.encode(s))
# a cipher that does not permute the alphabet is no good

class bad:
    # substitution cipher must always return
    # same # of chars
    def encode(self, s):
        return('x')
        
    def decode(self, s):
        return('xy')

class bad2:
    # substitution cipher must map all characters 1 to 1
    # here 'a' and 'b' both map to 'b'
    def encode(self, s):
        return(''.join([ 'b' if e == 'a' else e for e in s]))
    def decode(self, s):
        return(s)
#test goodness        
def goodperm(ob):
    alphabet = string.ascii_lowercase
    e=ob.encode(alphabet)
    if(alphabet==ob.decode(e)):
       return(True)
    else:
       return(False)
    
[goodperm(c) for c in [swapaz(), bad(), bad2()]]


#Problem 1b
import pickle

def saveKey(keypath, key):
    if(goodperm(key)==True):
      with open(keypath, 'bw') as f:
        pickle.dump(key, f)
      return None
    else:
        raise Exception('Expected a valid key')

keypath = '/tmp/key.pickle'
saveKey(keypath, swapaz())

#Problem 2a
def encode(keypath,string):  
    with open(keypath, 'br') as f:
        key= pickle.load(f)
    return(key.encode(string))
    
def decode(keypath,string):
    with open(keypath, 'br') as f:
        key= pickle.load(f)
    return(key.decode(string))

e = encode(keypath, "larry")
print(e)
decode(keypath, e)


#Problem 2b
class encrot(int):
      def __init__(self,n):
          self.index=n
      def encode(self,string):
              return(string[-self.index:]+string[:-self.index])
# rotate 3 chars to the right
e3 = encrot(3)
e3.encode(alphabet)
# rotate 5 chars to the left
em3 = encrot(-5)
em3.encode(alphabet)

#Problem 3
from collections import  defaultdict
import urllib
from bs4 import BeautifulSoup
def actors2(url):
      speach=urllib.request.urlopen(url)
      sp = BeautifulSoup(speach, 'html', from_encoding='utf-8')
      s=[]
      for x in sp.findAll('b'):
         if(str(x).find(':')!=-1):
             s.append(str(x)[3:-5])
         else:
             s.append(str(x)[3:-4])
      d=defaultdict(int)
      for k in s:
          d[k]+=1
      speach=urllib.request.urlopen(url)
      return([len(speach.readlines()),len(sp.findAll('b')),d])

# use this url for the hamlet text, don't hit MIT
url = 'https://courseworks.columbia.edu/access/content/group/COMSW3101_002_2015_3/week3/hamlet.html'
actors2(url)

#Problem 4

class Interval:
     def __init__(self,x,y):
         self.imin=x
         self.imax=y
     def __add__(self,p2):
        if isinstance(p2,Interval):
          return(Interval(p2.imin+self.imin,p2.imax+self.imax))
        else:
          return(Interval(p2+self.imin,p2+self.imax))
     def __str__(self):
         return('Interval<%d,%d>' % (self.imin,self.imax))
        
     def __repr__(self):
        return(self.__str__())
     def __mul__(self,p2):
         if isinstance(p2,Interval):
             a=self.imin*p2.imin
             b=self.imin*p2.imax
             c=self.imax*p2.imin
             d=self.imax*p2.imax
             return(Interval(min(a,b,c,d),max(a,b,c,d)))
         #right multiply    
         else:
             if(p2>0):
                 return(Interval(p2*self.imin,p2*self.imax))
             else:
                 return(Interval(p2*self.imax,p2*self.imin))
    
i = Interval(-1,6)
i2 = Interval(5, 13)
i3 = Interval(10,10)
[i + i2, i * i2, i + 10, i * 10, i + i3, i * i3]   
#Problem 5
#ploylist
import functools

class polylist: 
    ''' list poly representation'''
    def __init__(self, coe):
        self.coe = coe

    def termString(self, c , e):
        cs = str(c)
        if c > 0:
            cs = '+ ' + cs
        if (e == 0):
            return(cs)
        if (e == 1):
            return('%s*X' % cs)    
        return('%s*X**%d' % (cs,e))
        
    def __str__(self):
        # print math style - hard to do right
        terms = [self.termString(c,e) 
            for e,c in enumerate(self.coe) 
            if c != 0]
        terms.reverse()
        s = (' '.join(terms))
        # get rid of leading + 
        return(s)
        
    def __repr__(self):
        return(self.__str__())

    def __len__(self):
        # number of non zero terms
        # 0 len => bool false
        return(len(self.coe) - self.coe.count(0))

    def __add__(self, p2):
        p1len = len(self.coe)
        p2len = len(p2.coe)
        pad = p2len - p1len
        c1 = self.coe
        c2 = p2.coe
        
        if pad < 0:
            c1, c2 = c2, c1
            pad = -pad
    
        c1 = c1[:]
        
        c1.extend([0]*pad)
    
        return(polylist([t1+t2 for t1,t2 in zip(c1,c2)]))
    
    # don't allow a hash
    __hash__ = None    
    
    def evaluate(self, n):
        sum = 0
        for e,c in enumerate(self.coe):
            sum += c*n**e
        return(sum)
  
    def __mul__(self, p2):
        sums = []
        for e1,c1 in enumerate(self.coe):
            prod = [c1 * c2 for c2 in p2.coe]
            for rpt in range(e1):
                prod.insert(0, 0)
            sums.append(polylist(prod))
        return(functools.reduce(polylist.__add__, sums))
    def topolydict(self):
        b={x:self.coe[x-len(self.coe)] for x in reversed(range(0,len(self.coe))) } 
        return(polydict(b))
        
     
        
        

#polydict
class polydict:
    '''sparse poly representation using a dict
        sparse is {exponent:coefficient, ...}
        only non-zero terms appear in the dict
        
        {2:3, 1:2, 0:1} <=> 3*X**2 + 2*X + 1
    '''
    def __init__(self, d={}):

        # why the copy??
        self.sparse = d.copy()

    def printTerm(self, c ,e):
        cs = str(c)
        if c > 0:
            cs = '+ ' + cs
        if (e == 0):
            return(cs)
        if (e == 1):
            return('%s*X' % cs)    
        return('%s*X**%d' % (cs,e))   
        
    def __str__(self):
        if len(self.sparse) == 0:
            return('0')
        terms = [self.printTerm(self.sparse[e],e) 
                for e in sorted(self.sparse.keys(), 
                                reverse=True) 
                    if self.sparse[e] != 0]
        s = ' '.join(terms)
        if '+ ' == s[0:2]:
            s = s[2:]
        return (s)
    
    def __repr__(self):
        return(self.__str__())

    # don't allow a hash
    __hash__ = None  
    
    def __len__(self):
        return(len(self.sparse))

    # can explicity define bool
    def __bool__(self):
        return(False if len(self.sparse)==0 else True)
        
    def __iter__(self):
        # return a generator function that will
        # iterate thru (exp, coe) pairs
        return( (i for i in self.sparse.items() ))

    # should check types
    def __eq__(self, other):
        return(self.sparse == other.sparse)
        
    def __ne__(self, other):
        return(self.sparse != other.sparse)
        
    # define comparsion to be value of poly at 1
    def __lt__(self, other):
        return(self.evaluate(1) < other.evaluate(1))
        
    def __le__(self, other):
        return(self.evaluate(1) <= other.evaluate(1))
        
    # does poly 'contain' an exponent?
    def __contains__(self, e):
        return(e in self.sparse)

                
    def evaluate(self, n):
        '''eval poly at x=n'''
        sum = 0
        for e in self.sparse.keys():
            sum += self.sparse[e]*n**e
        return(sum)
            
    def __add__(self, p2):
        '''add two polys'''
        n = self.sparse.copy()
        for k,v in p2.sparse.items():
            if None == n.get(k):
                n[k] = v
            else:
                n[k] += v
        return(polydict(n))
        
    def __getitem__(self, index):
        '''pull out terms of the poly
           p[2], p[2:5]
           '''
        keys = sorted(self.sparse.keys(), reverse=True)
        if isinstance(index, int):
            # if asked for a single term, p[n], index will
            # be an int
            inds = [index]
        if isinstance(index, slice):
            # if asked for a slice, p[n:m], index will be
            # a 'slice' object
            inds = range(*index.indices(len(keys)))
        d = {}
        for i in inds:
            e = keys[i]
            d[e] = self.sparse[e]
        return(polydict(d))
        
    def __rmul__(self, p2):
        ''' multiple by a scalar on the right
            5*p1
            '''
        if isinstance(p2, int):
            nd = {}
        for e,c in self.sparse.items():
            nd[e] = c * p2 
        return(polydict(nd))
        
    def differentiate(self):
        d = {}
        for e,c in self.sparse.items():
            if e != 0:
                d[e-1] = c * e
        return(polydict(d))
    
    def integrate(self):
        d = {}
        for e,c in self.sparse.items():
            d[e+1] = c /(e+1.)
        return(polydict(d))
    def topolylist(self):
            s=''
            for x in self.sparse.keys():
                if(x<0):
                    s+=str(x)
            if(s!=''):
              raise ValueError("Negative exponent:%s"%s)
            else:
                l=[0 for x in range(max(self.sparse.keys())+1)]
                for a in sorted(self.sparse.keys()):
                    l[a]=self.sparse[a]
                return(polylist(l))
            
#test
pl1 = polylist([1,2,3])
[pl1, pl1.topolydict(), type(pl1.topolydict())]
pd1 = polydict({2:3, 1:2, 0:1})
[pd1, pd1.topolylist(), type(pd1.topolylist())]
#negative value may fail
pdn = polydict({-2:3})
pdn.topolylist() 



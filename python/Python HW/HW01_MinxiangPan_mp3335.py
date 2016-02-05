# Problem 1a
def dp(tv0,tv1):
    sum=0
    for n in range(0,len(tv0)):
            sum+=tv0[n]*tv1[n]
    if (type(tv0)!=list) |(type(tv1)!=list):
        print("Invalid argument")
    else:
         return(sum)
        
 #test 1#
tv0=[1,2,3]
tv1=[4,5,6]
dp(tv0,tv1)      
        
#test 2       
tv0=[1,2,3]
tv1=[4,5,6,7,8]
dp(tv0,tv1)

#Prblem 1B
def shortlong(tv0,tv1):
    if (type(tv0)!=list) |(type(tv1)!=list):
        print("Invalid argument")
    elif (len(tv0)<len(tv1)):
        return ([tv0,len(tv0),tv1,len(tv1)])
    else: 
        return ([tv1,len(tv1),tv0,len(tv0)])
#test 2       
tv0=[1,2,3]
tv1=[4,5,6,7,8,9]
shortlong(tv0,tv1)

#Problem 1C
def odp(tv0,tv1,o):
    #check the invalidaty of data
    l=shortlong(tv0,tv1)
    #make sure the vector would not out of boundary
    if(len(l[2][o:])<len(l[0])):
        print("vector is too short")
    else:
        return dp(l[0],l[2][o:])
        
[odp(tv0, tv1, 0), odp(tv0, tv1, 1), odp(tv0, tv1, 2)]

#Problem 1D
def pdp(tv0,tv1,o):
    #copy the list
    a=list(tv0)
    b=list(tv1)
    l=shortlong(a,b)
   
    for n in range(0,l[3]-l[1]):
          l[0].append(o)
    return dp(l[0],l[2])
    
[pdp(tv0, tv1, 0), pdp(tv0, tv1, 1), pdp(tv0, tv1,2)]

#Problem 2
l=[10,10,20,33,33,33,33,10,1,30,30,7]

def rle(l):
    a=set(l)
    s=[]
    for x in a:
        s.append([str(x),str(l.count(x))])
    return(s)
    
rle(l)

#Problem 3
def partition(l,n,o=0):
    s=[]
    if(type(l)==list):
     for x in range(0,round(len(l)/n)*(o+1)):
      if(x*n<o):
         s.append(l[(x*n):((x+1)*n)])
      else:     
           s.append(l[(x*(n-o)):((x+1)*n-o*x)])
     while(len(s[len(s)-1])<n):
         s=s[0:len(s)-1]

     return(s)
    elif(type(l)==range):
        for x in range(0,round(len(l)/n)*(o+1)):
            if(x*n<o):
               s.append(range((x*n),((x+1)*n)))
            else:     
               s.append(range(x*(n-o),((x+1)*n-o*x)))
        
        while(list(s[len(s)-1])[n-1]>l[len(l)-1]):
             s=s[0:len(s)-1]
        
        return(s)
#test       
l=list(range(0,10))       
partition(range(10),2,0)    
partition(l,2,0)
partition(l,2,1)
partition(l,4)
partition(l,4,3)

#Problem  4a
def expandlazy(o):
    if(type(o) in {range,zip,enumerate}):
        return (list(o))
    else:
        return(o)
#test        
[expandlazy(234), expandlazy(range(3)), expandlazy('asdf'), expandlazy(enumerate(['a','b','c']))]
#Problem  4b
def expandlazylist(o):
    s=[]
    for x in o:
     if(type(x) not in {int,float,str,complex}):
         s.append(list(x))
     else:
         s.append(x)
         
    return(s)
#test
ll = [1,2,3, range(4), 5, zip([1,2,3], [4,5]), 'asdf', enumerate(['a', 'b', 'c'])]
expandlazylist(ll)
#Problem  5b
def flatten(l):
    s=[]
    for x in l:
        if(type(x)==list):
            s.extend(flatten(x))
        else:
            s.append(x)
    return(s)
    
flatten( [1,[2,3,4,[5,6,[7,8],9],11]])
flatten([1,2,3,[4,56],[44,55],7,8])
        


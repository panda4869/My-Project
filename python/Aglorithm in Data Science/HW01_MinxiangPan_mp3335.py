# -*- coding: utf-8 -*-
"""
Created on Tue Sep 29 17:34:16 2015

@author: panminxiang
"""
#read data
import urllib2
homer = urllib2.urlopen('http://people.sc.fsu.edu/~jburkardt/datasets/sgb/homer.dat')

def read_nodes(gfile):
      s=[]# creat a new list
      address = gfile.geturl()
      c=urllib2.urlopen(address)
      for a in c.readlines():
         if(a.find(':')!=-1):# if there is ':' that is the line we want
         #eliminate '\n' begin with the first word after ':' split by ';'
            for edgeset in a.strip().split(':')[1].split(';'):
               for x in edgeset.split(','):#get the edges
                   if (x not in s):# select unique nodes 
                      s.append(x) 
      return (s)

       
def read_edges(gfile):
      s=[]# creat a new list
      address = gfile.geturl()
      c=urllib2.urlopen(address)
      for a in c.readlines():
         if(a.find(':')!=-1):# if there is ':' that is the line we want
         #eliminate '\n' begin with the first word after ':' split by ';'
            for edgeset in a.strip().split(':')[1].split(';'):
               for a in edgeset.split(','):#get the edges
                  for b in edgeset.split(','):
                      if (a !=b):# select unique nodes 
                          s.append((a,b))      
      return (s)
#if we want to use the function we have to urlopen the path each time     
import networkx as nx
G = nx.Graph() 
G.add_nodes_from(read_nodes(homer)) 
G.add_edges_from(read_edges(homer))

#DFS
def DFS(graph, root):
    #use a dictionary to record the exploration status
       explored={x:0 for x in graph.nodes()}
       explored[root]=1
       #use a list to record the visited nodes
       s=[root]
       #use recursive method to visit nodes
       def search(graph, root):
         if(graph.neighbors(root)==[]):
                return(s)
         else:
           n=0
          #check whether neighbors have been all visited
           for x in graph.neighbors(root):
               if(explored[x]==1):
                  n+=1
           if(n==len(graph.neighbors(root))):
                  return(s)# if all visited return the nodes
                  # otherwise explore each adjecent nodes
           for node in sorted(graph.neighbors(root)):
              if(explored[node]==0):
                 s.append(node)
                 explored[node]=1
                 search(graph,node)
       search(graph, root)
       return(s)
ulysses = DFS(G, 'OD')               
               
def connected_components(graph):
        explored={x:0 for x in graph.nodes()}
        s=[]
        for node in sorted(graph.nodes()):
             if(explored[node]==0):
                a=DFS(graph,node)                
                s.append(a)
                for b in a:
                    explored[b]=1
        return(s)
                
character_interactions =connected_components(G)   
       
component_sizes = [len(c) for c in character_interactions]
print "There are 12 connected components in the Iliad:", len(component_sizes) == 12
print "The giant component has size 542:", max(component_sizes) == 542
print "There are 5 isolated characters:", len([c for c in component_sizes if c == 1]) == 5            
             
             
     
             
               
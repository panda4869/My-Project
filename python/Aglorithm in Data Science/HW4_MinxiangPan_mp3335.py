# -*- coding: utf-8 -*-
"""
Created on Mon Nov 23 00:34:04 2015

@author: panminxiang
"""
import networkx as nx

def create_graph(infile):
     #initial directed graph
     G=nx.DiGraph()

     #open file
     with open(infile,'r') as f:
     
      for line in f:
         if line.startswith('p'):
             n=int(line.strip().split(" ")[2])
             
         if line.startswith('n'):
             c,node,demand=line.strip().split(" ")
             G.add_node(node,demand=int(demand))
            
         if line.startswith('a'):
             a,head,tail,low,cap,cost=line.strip().split(" ")
             if head not in G.nodes():
                 G.add_node(head,demand=0)
             if tail not in G.nodes():
                 G.add_node(tail,demand=0)
             if (head,tail) in G.edges():
                 newnode=str(n+1)
                 G.add_node(newnode,demand=0)
                 G.add_edge(head,newnode,weight=int(cost),capacity=int(cap))
                 G.add_edge(newnode,tail,weight=0,capacity=int(cap))
                 n+=1
             if (head,tail) not in G.edges():
                 G.add_edge(head,tail,weight=int(cost),capacity=int(cap))
     return G


G_40 = create_graph('gte_bad.40')
G_6830 = create_graph('gte_bad.6830')
G_176280 = create_graph('gte_bad.176280')



print "Correct value for _40 instance:", nx.min_cost_flow_cost(G_40) == 52099553858
print "Correct value for _6830 instance:", nx.min_cost_flow_cost(G_6830) == 299390431788
print "Correct value for _176280 instance:", nx.min_cost_flow_cost(G_176280) == 510585093810

import pulp

def lp_flow_value(G):
    nodes=[a for a in G.nodes() ]
    demand={a:G.node[a]['demand'] for a in G.nodes()}
    
    arcs=[(a,b) for(a,b) in G.edges()]
    
    arcdata={(a,b):[G.edge[a][b]['weight'],G.edge[a][b]['capacity']] for (a,b) in G.edges()}
        
    (weight,cap)=pulp.splitDict(arcdata)
    vars = pulp.LpVariable.dicts("Route",arcs,None,None,pulp.LpInteger)
    for a in arcs:
        vars[a].bounds(0, cap[a])
    
    prob = pulp.LpProblem("mincostflow",pulp.LpMinimize)
    prob += pulp.lpSum([vars[a]* weight[a] for a in arcs]), "Total Cost of Transport"
    
    for n in nodes:
        prob += (pulp.lpSum([vars[(i,j)] for (i,j) in arcs if j == n]) - pulp.lpSum([vars[(i,j)] for (i,j) in arcs if i == n])==demand[n]), \
                "Flow Conservation in Node %s"%n
    prob.solve()
    return int(pulp.value(prob.objective))

print "Correct value for _40 instance:", lp_flow_value(G_40) == 52099553858
print "Correct value for _6830 instance:", lp_flow_value(G_6830) == 299390431788
print "Correct value for _176280 instance:", lp_flow_value(G_176280) == 510585093810




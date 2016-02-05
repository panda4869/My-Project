# -*- coding: utf-8 -*-
"""
Created on Thu Oct 29 12:04:00 2015

@author: panminxiang
"""
import networkx as nx
G = nx.Graph()

usa = open('contiguous-usa.dat')
for line in usa:
    s1, s2 = line.strip().split()
    G.add_edge(s1, s2)

#encode demands
for state in G.nodes():
    if state != 'CA':
        G.node[state]['demand'] = 1
G.node['CA']['demand'] = -48

#add capacity and convert to directed graph
G = nx.DiGraph(G)
uniform_capacity = 16
for (s1, s2) in G.edges():
    G.edge[s1][s2]['capacity'] = uniform_capacity
    
        
        
        
def flow_with_demands(G):
    """Computes a flow with demands over the given graph.
    
    Args:
        graph: A directed graph with nodes annotated with 'demand' properties and edges annotated with 'capacity' 
            properties.
        
    Returns:
        A dict of dicts containing the flow on each edge. For instance, flow[s1][s2] should provide the flow along
        edge (s1, s2).
        
    Raises:
        NetworkXUnfeasible: An error is thrown if there is no flow satisfying the demands.
    """
    # TODO: Implement the function.
    def flowreturn(flow,G):
        GH=G.copy()
        for edge in G.edges():
            s1,s2=edge
            GH.edge[s1][s2]['flow']=flow[s1][s2]
        return GH.edge
            
    GC = G.copy()

    # add new source and terminal nodes
    GC.add_node('source')
    GC.add_node('terminal')
    
    # add edges of required capacity
    for node in G.nodes():
        demand = G.node[node]['demand']
        if demand < 0:
            GC.add_edge('source', node)
            obj=GC['source'][node]['capacity'] = -demand
        if demand > 0:
            GC.add_edge(node, 'terminal')
            GC[node]['terminal']['capacity'] = demand
    flow_value, flow_with_demands = nx.maximum_flow(GC, 'source', 'terminal')
    if(flow_value!=obj):
       raise  nx.NetworkXUnfeasible("No flow satisfying the demands ")
    else:    
     return flowreturn(flow_with_demands,G)
        

    
def divergence(flow):
    """Computes the total flow into each node according to the given flow dict.
    
    Args:
        flow: the flow dict recording flow between nodes.
        
    Returns:
        A dict of the net flow into each node.
    """
    # TODO: Implement the function.
    s={x:0 for x in flow}
    for s1 in flow:
        
        for s2 in flow[s1]:
            if(s1!=s2):
               s[s1]+=flow[s2][s1]['flow']-flow[s1][s2]['flow']
    
    return(s)    



flow=flow_with_demands(G)
div = divergence(flow)

print "Flow satisfies all demands:", all(div[n] == G.node[n]['demand'] for n in G.nodes())



# -*- coding: utf-8 -*-
"""
Created on Mon Nov 30 18:54:45 2015

@author: panminxiang
"""

import networkx as nx
import pulp

karate = nx.read_gml('karate.gml')
power = nx.read_gml('power.gml')


        
def independent_set_ip(graph):
    G=graph.copy() 
    node=[a for a in G.nodes()]
    vars = pulp.LpVariable.dicts("Node",node,0,1,pulp.LpInteger)
    prob = pulp.LpProblem("Maximum IS",pulp.LpMaximize)
    prob += pulp.lpSum([vars[a] for a in node]), "Total Cost of Transport"
    
    for (i,j) in G.edges():
        prob += vars[i]+vars[j]<=1
    
    prob.solve()
    s=[]
    for v in prob.variables():
        s.append((int(v.name[5:]),int(v.varValue)))
        
    return s
    
def set_weight(solution):
    """Computes the total weight of the solution of an LP or IP for independent set.
    
    Args:
      - solution (list[int, float]): the LP or IP solution
    
    Returns:
      (float) Total weight of the solution
    
    """
    return sum(value for (node, value) in solution)
    
    
    
karate_ind_set = independent_set_ip(karate)
print "Size of karate set = ", set_weight(karate_ind_set)
power_ind_set = independent_set_ip(power)
print "Size of power set = ", set_weight(power_ind_set)


def independent_set_lp(graph):
    G=graph.copy() 
    node=[a for a in G.nodes()]
    vars = pulp.LpVariable.dicts("Node",node,0,1)
    prob = pulp.LpProblem("Maximum IS",pulp.LpMaximize)
    prob += pulp.lpSum([vars[a] for a in node]), "Total Cost of Transport"
    
    for (i,j) in G.edges():
        prob += vars[i]+vars[j]<=1
    
    prob.solve()
    s=[]
    for v in prob.variables():
        s.append((int(v.name[5:]),float(v.varValue)))
        
    return s
    
    
  
karate_ind_set_relax = independent_set_lp(karate)
print "Value of karate set = ", set_weight(karate_ind_set_relax)
power_ind_set_relax = independent_set_lp(power)
print "Value of power set = ", set_weight(power_ind_set_relax)    
    

def round_solution(solution, graph):
    G=graph.copy() 
    for v in solution:
        if v[1]<=0.5:
            G.remove_node(v[0])
    return G       
    




def solution_quality(rounded, optimal):
    """Computes the percent optimality of the rounded solution.
    
    Args:
      - rounded (nx.Graph): the graph obtained from rounded LP solution
      - optimal: size of maximum independent set
    
    """
    num_nodes = rounded.number_of_nodes() - rounded.number_of_edges()
    return float(num_nodes) / optimal

 
karate_rounded = round_solution(karate_ind_set_relax, karate)
karate_quality = solution_quality(karate_rounded, set_weight(karate_ind_set))
print "Quality of karate rounded solution = {:.0f}%".format(karate_quality*100)

power_rounded = round_solution(power_ind_set_relax, power)
power_quality = solution_quality(power_rounded, set_weight(power_ind_set))
print "Quality of power rounded solution = {:.0f}%".format(power_quality*100)

   
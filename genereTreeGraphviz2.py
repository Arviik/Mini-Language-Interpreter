# -*- coding: utf-8 -*-
"""
Author : Vincent Genin ESGI-3AL 2018
"""

import uuid
import graphviz as gv


def print_tree_graph(t):
    graph = gv.Digraph(format='pdf')
    graph.attr('node', shape='circle')
    add_node(graph, t)
    graph.render(filename='img/graph')  # Pour Sauvegarder
    graph.view()  # Pour afficher


def add_node(graph, t):
    my_id = uuid.uuid4()

    if type(t) is not tuple:
        graph.node(str(my_id), label=str(t))
        return my_id

    graph.node(str(my_id), label=str(t[0]))
    for i in range(1, len(t)):
        graph.edge(str(my_id), str(add_node(graph, t[i])), arrowsize='0')

    return my_id


print_tree_graph(('+', 1, 2))

#!/usr/bin/env python3
import sys
import random

nnodes = int(sys.argv[1])
nedges = int(sys.argv[2])

def randedge(): 
    return (random.randrange(nnodes), random.randrange(nnodes))

edges = set()
while len(edges) < nedges:
    edge = randedge()
    edges.add(edge)

for (a, b) in sorted(edges):
    print(a, b)

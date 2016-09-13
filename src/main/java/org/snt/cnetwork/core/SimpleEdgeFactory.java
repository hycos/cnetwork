package org.snt.cnetwork.core;

public class SimpleEdgeFactory implements org.jgrapht.EdgeFactory<Node,Edge> {

    public Edge createEdge(Node v1, Node v2) {
        return new Edge(v1, v2);
    }
}
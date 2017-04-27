/**
 * This file is part of the Joana IFC project. It is developed at the
 * Programming Paradigms Group of the Karlsruhe Institute of Technology.
 *
 * For further details on licensing please read the information at
 * http://joana.ipd.kit.edu or contact the authors.
 */
package org.snt.cnetwork.core.graph;

import org.jgrapht.DirectedGraph;
import org.jgrapht.EdgeFactory;
import org.jgrapht.graph.DirectedPseudograph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

public class AbstractGraph implements DirectedGraph<Node, Edge>, Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(AbstractGraph.class);

    private final DirectedPseudograph<Node, Edge> delegate;

    public AbstractGraph() {
        this.delegate = new DirectedPseudograph(Edge.class);
    }

    public boolean addEdge(Node arg0, Node arg1, Edge arg2) {
        return delegate.addEdge(arg0, arg1, arg2);
    }

    public Edge addEdge(Node arg0, Node arg1) {
        return delegate.addEdge(arg0, arg1);
    }

    public boolean addVertex(Node arg0) {
        return delegate.addVertex(arg0);
    }

    public boolean containsEdge(Edge arg0) {
        return delegate.containsEdge(arg0);
    }

    public boolean containsEdge(Node arg0, Node arg1) {
        return delegate.containsEdge(arg0, arg1);
    }

    public boolean containsVertex(Node arg0) {
        return delegate.containsVertex(arg0);
    }

    public int degreeOf(Node arg0) {
        return delegate.degreeOf(arg0);
    }


    public Set<Edge> edgeSet() {
        return delegate.edgeSet();
    }


    public Set<Edge> edgesOf(Node arg0) {
        return delegate.edgesOf(arg0);
    }

    @SuppressWarnings("unchecked")
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (obj == null) {
            return false;
        } else if (!(obj instanceof AbstractGraph)) {
            return false;
        } else {
            AbstractGraph other = (AbstractGraph) obj;
            DirectedPseudograph<Node, Edge> otherDelegate = (DirectedPseudograph<Node, Edge>) other.delegate;
            return delegate.equals(otherDelegate);
        }
    }


    public Set<Edge> getAllEdges(Node arg0, Node arg1) {
        return delegate.getAllEdges(arg0, arg1);
    }

    public Edge getEdge(Node arg0, Node arg1) {
        return delegate.getEdge(arg0, arg1);
    }

    public EdgeFactory<Node, Edge> getEdgeFactory() {
        return delegate.getEdgeFactory();
    }

    public Node getEdgeSource(Edge arg0) {
        return delegate.getEdgeSource(arg0);
    }

    public Node getEdgeTarget(Edge arg0) {
        return delegate.getEdgeTarget(arg0);
    }

    public double getEdgeWeight(Edge arg0) {
        return delegate.getEdgeWeight(arg0);
    }

    public int hashCode() {
        return delegate.hashCode();
    }

    public int inDegreeOf(Node arg0) {
        return delegate.inDegreeOf(arg0);
    }

    public Set<Edge> incomingEdgesOf(Node arg0) {
        Set<Edge> ret = new TreeSet<>();
        try {
            ret.addAll(delegate.incomingEdgesOf(arg0));
        } catch (NullPointerException e) {
            LOGGER.error("Could not find {}", arg0.getId());
            return ret;
        } finally {
            return ret;
        }
    }

    public boolean isAllowingLoops() {
        return delegate.isAllowingLoops();
    }

    public boolean isAllowingMultipleEdges() {
        return delegate.isAllowingMultipleEdges();
    }

    public int outDegreeOf(Node arg0) {
        return delegate.outDegreeOf(arg0);
    }

    public Set<Edge> outgoingEdgesOf(Node arg0) {

        Set<Edge> ret = new HashSet();
        try {
            ret.addAll(delegate.outgoingEdgesOf(arg0));
        } catch (NullPointerException e) {
            LOGGER.error(e.getMessage());
            return ret;
        } finally {
            return ret;
        }
    }

    public boolean removeAllEdges(Collection<? extends Edge> arg0) {
        return delegate.removeAllEdges(arg0);
    }

    public Set<Edge> removeAllEdges(Node arg0, Node arg1) {
        return delegate.removeAllEdges(arg0, arg1);
    }

    public boolean removeAllVertices(Collection<? extends Node> arg0) {
        return delegate.removeAllVertices(arg0);
    }

    public boolean removeEdge(Edge arg0) {
        return delegate.removeEdge(arg0);
    }

    public Edge removeEdge(Node arg0, Node arg1) {
        return delegate.removeEdge(arg0, arg1);
    }

    public boolean removeVertex(Node arg0) {
        return delegate.removeVertex(arg0);
    }


    public void setEdgeWeight(Edge arg0, double arg1) {
        delegate.setEdgeWeight(arg0, arg1);
    }

    public String toString() {
        return delegate.toString();
    }

    public Set<Node> vertexSet() {
        return delegate.vertexSet();
    }

    @Override
    public AbstractGraph clone(){
        AbstractGraph an = new AbstractGraph();
        for(Edge e : this.edgeSet()) {
            Node src = e.getSrcNode().clone();
            Node dest = e.getDestNode().clone();
            Edge ne = new Edge(src,dest,e.getSequence());
            an.addEdge(src,dest,ne);
        }
        return an;
    }

}

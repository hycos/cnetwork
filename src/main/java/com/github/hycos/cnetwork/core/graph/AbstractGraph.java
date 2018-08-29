/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

/**
 * This file is part of the Joana IFC project. It is developed at the
 * Programming Paradigms Group of the Karlsruhe Institute of Technology.
 *
 * For further details on licensing please read the information at
 * http://joana.ipd.kit.edu or contact the authors.
 */
package com.github.hycos.cnetwork.core.graph;

import org.jgrapht.EdgeFactory;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DirectedPseudograph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

public class AbstractGraph extends DefaultDirectedGraph<Node, Edge> implements Cloneable, Serializable {

    private static final long serialVersionUID = -7824222790097211310L;

    final static Logger LOGGER = LoggerFactory.getLogger(AbstractGraph.class);

    private final DirectedPseudograph<Node, Edge> delegate;

    public AbstractGraph() {
        super(Edge.class);
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

}

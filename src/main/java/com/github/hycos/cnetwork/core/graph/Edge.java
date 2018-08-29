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

package com.github.hycos.cnetwork.core.graph;

import com.github.hycos.cnetwork.api.EdgeInterface;
import org.jgrapht.graph.DefaultEdge;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;

public class Edge extends DefaultEdge implements Comparable<Edge>, Cloneable,
        EdgeInterface, Serializable {

    private static final long serialVersionUID = -8824622790097211310L;

    final static Logger LOGGER = LoggerFactory.getLogger(Edge.class);

    private Node srcNode;
    private Node destNode;
    private EdgeKind kind;
    private int sequence;
    private int id;
    protected ConstraintNetwork cn = null;


    public Edge(ConstraintNetwork cn,
                Node srcNode,
                Node dstNode,
                EdgeKind kind,
                int sequence) {
        this.srcNode = srcNode;
        this.destNode = dstNode;
        this.kind = kind;
        this.sequence = sequence;
        this.cn = cn;
        this.id = cn.nextEdgeId();
    }

    public Edge(Edge e) {
        this(null, e.getSrcNode(), e.getDestNode(), e.getSequence());
        this.kind = e.getKind();
        this.id = e.id;
    }

    public Edge(ConstraintNetwork cn,
                Node srcNode,
                Node dstNode,
                int sequence) {
        this(cn, srcNode,dstNode,EdgeKind.PAR_IN,sequence);
    }

    public Edge(ConstraintNetwork cn,
                Node srcNode,
                Node dstNode) {
        this(cn, srcNode,dstNode,EdgeKind.PAR_IN,0);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{" + this.id + "}[");
        sb.append(srcNode.getId());
        sb.append("-(" + sequence + ":");
        sb.append(this.kind.toString());
        sb.append(")->");
        sb.append(destNode.getId() + "]");
        return sb.toString();
    }

    @Override
    public int compareTo(Edge o) {
        return this.sequence - o.sequence;
    }

    public EdgeKind getKind() {
        return kind;
    }

    public void setKind(EdgeKind kind) {
        this.kind = kind;
    }

    public int  getSequence() {
        return this.sequence;
    }

    @Override
    public int getId() {
        return this.id;
    }

    @Override
    public int hashCode() {
        //int hc = kind.hashCode();
        //hc = 37 * hc + getSrcNode().hashCode();
        //return 37 * hc + getDestNode().hashCode();
        return this.id;
    }

    @Override
    public boolean equals(Object o) {

        if(!(o instanceof Edge))
            return false;

        Edge e = (Edge)o;

        //return this.getKind() == e.getKind() && this.getSrcNode().id == e.getSrcNode().id && this.getDestNode().id == e.getDestNode().id;
        return this.id == e.id;
    }

    public Node getSrcNode() {
        return srcNode;
    }

    public void setSrcNode(Node srcNode) {
        this.srcNode = srcNode;
    }

    public Node getDestNode() {
        return destNode;
    }

    public void setDestNode(Node destNode) {
        this.destNode = destNode;
    }

    @Override
    public Node getSource() {
        return this.srcNode;
    }

    @Override
    public Node getTarget() {
        return this.destNode;
    }

    @Override
    public Edge clone() {
        return new Edge(this);
    }
}


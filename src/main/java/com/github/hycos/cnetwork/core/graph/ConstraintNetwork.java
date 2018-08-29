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

import com.github.hycos.cnetwork.api.NodeKindInterface;
import com.github.hycos.cnetwork.sig.JavaMethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class ConstraintNetwork extends AbstractGraph implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetwork.class);

    // keep track of sat edges
    private HashSet<Edge> sat = new HashSet<>();
    private HashSet<Edge> unsat = new HashSet<>();
    private HashMap<String, Operation> opLookup = new HashMap<>();

    private int nid = 0;
    private int eid = 0;


    // this is usually null - we just use it for thread models
    private Node startNode = null;

    private String variablePfx = "_v";
    private int vidx = 0;

    public int nextNodeId() {
        return nid ++;
    }

    public int nextEdgeId() {
        return eid ++;
    }

    public ConstraintNetwork() {
    }


    protected ConstraintNetwork(ConstraintNetwork other) {
        for (Node n : other.vertexSet()) {

            Node nn = n.clone();
            nn.cn = this;
            this.addVertex(n.clone());
        }
        for (Edge e : other.edgeSet()) {
            Node src = this.getNodeById(e.getSrcNode().getId());
            Node dest = this.getNodeById(e.getDestNode().getId());
            Edge ne = new Edge(this, src, dest, e.getSequence());
            this.addConnection(ne);
        }

        this.nid = other.nid;
        this.eid = other.eid;
        //dctrl = other.dctrl.clone();
        //buildNodeIdx();
    }

    protected Set<Edge> getAllConnectedEdges(Node n) {
        Set<Edge> ret = new HashSet<>();

        ret.addAll(super.incomingEdgesOf(n));
        ret.addAll(super.outgoingEdgesOf(n));

        return ret;
    }

    public Node getNodeById(int id) {

        Node ret = null;

        for (Node n : this.vertexSet()) {
            if (n.getId() == id)
                ret = n;
        }

        assert (ret != null);
        return ret;
    }

    public ConstraintNetwork subgraph(Collection<Node> vertices) {
        ConstraintNetwork g = new ConstraintNetwork();

        for (Node n : vertices) {
            //LOGGER.debug("add vertex {}", n.getId());
            g.addVertex(n);
        }

        for (Node n : vertices) {
            for (Edge e : outgoingEdgesOf(n)) {
                if (vertices.contains(e.getDestNode())) {
                    g.addEdge(e);
                }
            }
        }
        return g;
    }

    protected boolean addEdge(Edge edge) {
        return super.addEdge(edge.getSrcNode(), edge.getDestNode(), edge);
    }


    protected Node getStartNode() {
        return this.startNode;
    }

    protected void setStartNode(Node startNode) {
        this.startNode = startNode;
    }

    protected void unsetStartNode() {
        this.startNode = null;
    }

    protected List<Edge> getIncomingEdgesOfKind(Node n, EdgeKind kind) {

        LinkedList<Edge> ret = new LinkedList<Edge>();

        Set<Edge> allIncoming = incomingEdgesOf(n);

        if (allIncoming == null)
            return null;

        for (Edge e : allIncoming) {
            //LOGGER.info("EEEE " + e.toString());
            if (e.getKind() == kind) {
                ret.add(e);
            }
        }

        // sort parameter Order
        Collections.sort(ret);

        return ret;
    }

    protected List<Node> getParametersFor(Node n) {
        List<Node> params = new Vector<>();

        if (n instanceof Operation) {
            for (Edge paredge : this.incomingEdgesOf(n)) {
                params.add(paredge.getSrcNode());
            }
        }
        return params;
    }


    protected Operand getAuxiliaryVariable(DefaultNodeKind kind) {
        Operand op = new Operand(this, variablePfx + (vidx++), kind);
        addNode(op);
        return op;
    }


    protected Node addOperand(NodeKindInterface kind, String label) {
        //Node n = this.getNodeByLabel(label);
        //if (n == null) {
        Node n = new Operand(this, label, kind);
        addNode(n);
        return n;
    }

    protected Operation registerExtOperation(String bytecodesig,
                                             NodeKindInterface ki,  String
                                                     label) {

        //LOGGER.info("bytecodesig " + bytecodesig);
        //LOGGER.info("name " + label);

        if (opLookup.containsKey(bytecodesig))
            return null;

        JavaMethodSignature sig = JavaMethodSignature.fromString(bytecodesig);


        Operation op = new Operation(this, label, ki, sig);
        opLookup.put(label, op);

        //LOGGER.info("OPLOOKUP " + op.toString());

        return op;
    }

    protected Operation getExtOperation(String label) {
        if (!this.opLookup.containsKey(label))
            return null;
        return this.opLookup.get(label);
    }

    protected Operation addExtOperation(String identifier, List<Node> params) {

        Operation ext = getExtOperation(identifier);

        if (ext == null)
            return null;

        Operation op = new Operation(this, ext);

        linkParams(op, params);
        addNode(op);
        return op;
    }

    private void linkParams(Node op, List<Node> params) {
        for (int i = 0; i < params.size(); i++) {

            Node par = params.get(i);

            this.addConnection(par, op, EdgeKind.PAR_IN, i);
        }
    }

    protected Set<Node> getConnectedInNodes(Node n) {
        Set<Node> ret = new HashSet<>();
        Set<Edge> incoming;
        if ((incoming = incomingEdgesOf(n)) != null) {
            for (Edge e : incoming) {
                ret.add(e.getSrcNode());
            }
        }
        return ret;
    }

    protected Set<Node> getConnectedOutNodes(Node n) {
        Set<Node> ret = new HashSet<>();
        Set<Edge> outgoing;
        if ((outgoing = outgoingEdgesOf(n)) != null) {
            for (Edge e : outgoing) {
                ret.add(e.getDestNode());
            }
        }
        return ret;
    }


    protected Node addOperation(NodeKindInterface kind, Node... params) {
        List<Node> lst = Arrays.asList(params);
        return createOperation(kind, lst);
    }

    protected Node addOperation(NodeKindInterface kind, List<Node> params) {
        LOGGER.debug("create op {}", kind);
        return createOperation(kind, params);
    }


    private Node createOperation(NodeKindInterface kind, List<Node> params) {
        Node op = new Operation(this, kind);
        addNode(op);
        linkParams(op, params);
        return op;
    }


    protected Edge addConnection(Node src, Node target, EdgeKind kind, int priority) {
        Edge e = new Edge(this, src, target, kind, priority);
        addConnection(e);
        return e;
    }

    protected Edge addConnection(Edge e) {
        addVertex(e.getSrcNode());
        addVertex(e.getDestNode());
        super.addEdge(e.getSrcNode(), e.getDestNode(), e);

        return e;
    }

    protected void addConnections(Set<Edge> edges) {
        for (Edge e : edges) {
            addConnection(e);
        }
    }

    @Override
    public boolean removeAllVertices(Collection<? extends Node> arg0) {

        int size = vertexSet().size();
        for (Node n : arg0) {
            removeVertex(n);
        }
        return this.vertexSet().size() < size;
    }

    @Override
    public boolean removeVertex(Node n) {


        //LOGGER.debug(toDot());
        if (containsVertex(n)) {
            //nodeLookup.remove(n.getLabel());
            return super.removeVertex(n);
        } else {
            return false;
        }
    }


    @Override
    public boolean addVertex(Node n) {
        return super.addVertex(n);
    }

    protected Node addNode(Node n) {

        addVertex(n);
        return n;
    }


    protected Collection<Node> getAllVariables() {
        Set<Node> variables = new HashSet<Node>();
        for (Node n : this.vertexSet()) {
            if (n.isOperand() && !n.isLiteral())
                variables.add(n);
        }
        return variables;
    }


    protected void join(NodeKindInterface kind, Node cpoint, ConstraintNetwork
            othercn) {


        assert (othercn.getStartNode() != null);
        LOGGER.info("Start node is " + othercn.getStartNode().toString());

        for (Node n : othercn.vertexSet()) {
            this.addNode(n);
        }

        for (Edge e : othercn.edgeSet()) {
            this.addConnection(e);
        }

        //Node snode = this.getNodeByLabel(othercn.getStartNode().getLabel());

        //assert (this.vertexSet().contains(cpoint));

//        Node n = this.addOperation(kind, cpoint, othercn.getStartNode());
//        try {
//            n.setDomain(NodeDomainFactory.DBTRUE);
//        } catch (EUFInconsistencyException e) {
//            e.printStackTrace();
//        }
    }


    public String toDot() {

        StringBuilder sb = new StringBuilder();

        sb.append("digraph {\n" +
                "\trankdir=TB;\n");

        sb.append("\tnode [fontname=Helvetica,fontsize=11];\n");
        sb.append("\tedge [fontname=Helvetica,fontsize=10];\n");

        String shape = "";
        String label = "";
        String color = "";

        for (Node n : this.vertexSet()) {
            String kind = "";
            if (n instanceof Operand) {
                kind = n.getKind().toString();
            } else {
                kind = n.getKind().toString();
            }

            if (n.isOperation()) {
                shape = "box";
                label = "label";

                if (n.isConstraint()) {
                    color = "blue";
                } else {
                    color = "green";
                }

                if (n.getAnnotation().equals("unsat")) {
                    color = "red";
                }

            } else {
                shape = "ellipse";
                label = "label";
                color = "black";
            }


            sb.append("\tn" + n.getId() + " [color=" + color + ",shape=\"" +
                    shape + "\"," + label + "=\"" +
                    kind + "\\n" + n.getDotLabel() + "\"];\n");
        }

        String option = "";
        String ecolor = "black";
        String par = "";

        for (Edge e : this.edgeSet()) {
            Node src = e.getSrcNode();
            Node dest = e.getDestNode();

            assert (outgoingEdgesOf(src).contains(e));
            assert (incomingEdgesOf(dest).contains(e));

            assert (src != null);
            assert (dest != null);

            if (e.getKind() == EdgeKind.PAR_IN) {
                par = " " + e.getSequence();
            } else {
                par = "";
            }

            if (this.sat.contains(e)) {
                ecolor = "green";
            } else if (this.unsat.contains(e)) {
                ecolor = "red";
            } else {
                ecolor = "black";
            }

            sb.append("\tn" + src.getId() + " -> n" + dest.getId() +
                    "[color=\"" + ecolor + "\",label=\"p" + par.trim() + "\"" + option + "];\n");

        }

        sb.append("}");

        return sb.toString();
    }


    public String toConfig() {

        StringBuilder sb = new StringBuilder();

        LinkedList<Node> other = new LinkedList<Node>();
        LinkedList<Operand> links = new LinkedList<Operand>();


        for (Node n : this.vertexSet()) {
            if (n.isOperand() && !n.isLiteral() && !n.isRegex()) {

                String type;

                if (n.isBoolean()) {
                    type = "bool";
                } else if (n.isString()) {
                    type = "string";
                } else {
                    type = "int";
                }

                if (n.getKind().isThreatModel()) {
                    links.add((Operand) n);
                }

                sb.append("var " + type + " " + n.getLabel() + ";\n");
            } else if (n.isOperation() && n.getKind() == DefaultNodeKind.EXTERNAL) {
                sb.append("fun " + "\"" + n.getLabel() + "\";\n");
            } else if (n.isBoolean() && !n.isLiteral()) {
                if (outgoingEdgesOf(n) == null || outgoingEdgesOf(n).size() == 0) {
                    other.add(n);
                }
            }
        }

        // handle Threat Models

        sb.append("\n");
        for (Operand o : links) {
            sb.append("&(" + o.getLabel() + "," + o.getKind().getValue() + ");");
        }

        sb.append("\n");

        while (!other.isEmpty()) {
            Node o = other.poll();
            sb.append(o.getLabel() + ";\n");
        }

        return sb.toString();
    }

    @Override
    public ConstraintNetwork clone() {
        return new ConstraintNetwork(this);
    }


}

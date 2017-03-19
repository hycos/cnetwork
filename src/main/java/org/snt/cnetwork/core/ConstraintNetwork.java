package org.snt.cnetwork.core;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.sig.JavaMethodSignature;

import java.util.*;

public class ConstraintNetwork extends AbstractGraph implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetwork.class);

    // keep track of sat edges
    private HashSet<Edge> sat = new HashSet<>();
    private HashSet<Edge> unsat = new HashSet<>();

    // just used during construction
    //private HashMap<String, Node> nodeLookup = new HashMap<>();
    private HashMap<String, Operation> opLookup = new HashMap<>();

    // this is usually null - we just use it for thread models
    private Node startNode = null;

    private String variablePfx = "_v";
    private int vidx = 0;

    protected ConstraintNetwork() {}

    protected ConstraintNetwork(ConstraintNetwork other) {
        for(Node n : other.vertexSet()) {
            this.addVertex(n.clone());
        }
        for(Edge e : other.edgeSet()) {
            Node src = this.getNodeById(e.getSrcNode().getId());
            Node dest = this.getNodeById(e.getDestNode().getId());
            Edge ne = new Edge(src,dest,e.getSequence());
            this.addConnection(ne);
        }
        //buildNodeIdx();
    }

    protected Set<Edge> getAllConnectedEdges(Node n) {
        Set<Edge> ret = new HashSet<Edge>();

        ret.addAll(super.incomingEdgesOf(n));
        ret.addAll(super.outgoingEdgesOf(n));

        return ret;
    }

    public Node getNodeById(int id) {

        Node ret = null;

        for(Node n : this.vertexSet()) {
            if(n.getId() == id)
                ret = n;
        }

        assert(ret != null);
        return ret;
    }

    protected ConstraintNetwork subgraph(Collection<Node> vertices) {
        ConstraintNetwork g = new ConstraintNetwork();

        for (Node n : vertices) {
            LOGGER.debug("add vertex {}" , n.getId());
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


    protected Operand getAuxiliaryVariable(NodeKind kind) {
        Operand op = new Operand(variablePfx + (vidx++), kind);
        addNode(op);
        return op;
    }

    protected Node addOperand(NodeKind kind, String label) {
        //Node n = this.getNodeByLabel(label);
        //if (n == null) {
        Node n = new Operand(label, kind);
        addNode(n);
        return n;
    }

    protected Operation registerExtOperation(String bytecodesig, String label) {

        //LOGGER.info("bytecodesig " + bytecodesig);
        //LOGGER.info("name " + label);

        if (opLookup.containsKey(bytecodesig))
            return null;

        JavaMethodSignature sig = JavaMethodSignature.fromString(bytecodesig);


        Operation op = new Operation(label, sig);
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

        //String label = ext.getLabel() + "(" + getParameterList(params) + ")";

        //Operation op = (Operation) getNodeByLabel(label);

        //if (op == null) {
        Operation  op = new Operation(ext);
         //   op.setLabel(label);
        //} else {
        //    return op;
        //}

        linkParams(op, params);
        addNode(op);
        return op;
    }


    /**public String getLabel(NodeKind kind, List<Node> params) {
        return kind.toString() + "(" + getParameterList(params) + ")";
    }

    private String getParameterList(List<Node> params) {

        String label = "";

        for (int i = 0; i < params.size(); i++) {
            Node par = params.get(i);
            assert (par != null);
            //LOGGER.info("Par " + par.getKind());
            //LOGGER.info("Par" + par.getId());
            //LOGGER.info("+SPLIT " + par.getLabel());
            String plbl = par.getLabel();
            if (par instanceof Operand && par.isLiteral() && par.isString()) {
                plbl = par.getLabel();
            }

            label += plbl + (i < params.size() - 1 ? "," : "");
        }

        return label;
    }**/

    private void linkParams(Node op, List<Node> params) {
        for (int i = 0; i < params.size(); i++) {

            //assert vertexSet().contains(params.get(i));

            Node par = params.get(i);
            //Node par = this.getNodeByLabel(params.get(i).getLabel());
            //if (par == null) {
            //    par = addNode(params.get(i));
            //}
            this.addConnection(par, op, EdgeKind.PAR_IN, i);
        }
    }

    protected Set<Node> getConnectedInNodes(Node n) {
        Set<Node> ret = new HashSet<Node>();
        Set<Edge> incoming;
        if((incoming = incomingEdgesOf(n)) != null) {
            for(Edge e : incoming) {
                ret.add(e.getSrcNode());
            }
        }
        return ret;
    }

    protected Set<Node> getConnectedOutNodes(Node n) {
        Set<Node> ret = new HashSet<Node>();
        Set<Edge> outgoing;
        if((outgoing = outgoingEdgesOf(n)) != null) {
            for(Edge e : outgoing) {
                ret.add(e.getDestNode());
            }
        }
        return ret;
    }


    protected Node addOperation(NodeKind kind, Node... params) {
        List<Node> lst = Arrays.asList(params);
        return createOperation(kind, lst);
    }

    protected Node addOperation(NodeKind kind, List<Node> params) {
        return createOperation(kind, params);
    }


    private Node createOperation(NodeKind kind, List<Node> params) {
        //LOGGER.info("PARAMS LEN " + params.size());
        //Operation op;
        //String label = getLabel(kind, params);
        //LOGGER.info("LABEL IS " + label);
        //op = (Operation) getNodeByLabel(label);
        //if (op == null) {
        Node op = new Operation(kind);
        addNode(op);
        linkParams(op, params);
         //   assert nodeLookup.containsKey(op.getLabel());
        //}
        return op;
    }



    protected Edge addConnection(Node src, Node target, EdgeKind kind, int priority) {
        Edge e = new Edge(src, target, kind, priority);
        addConnection(e);
        return e;
    }

//    private String getNewLabelFor(Node n) {
//        List<Node> pars = getParametersFor(n);
//        String label = n.getKind().toString() + "(" + getParameterList(pars) +
//                ")";
//        return label;
//    }


//    protected Edge addConnection(Edge e) {
//
//        Edge r = addConnection(e);
//        if(!updateLbl) {
//            return r;
//        } else {
//            LOGGER.debug("{} -> {}", r.getSrcNode().getLabel(), r.getDestNode().getLabel());
//            LinkedList<Node> worklist = new LinkedList<>();
//            worklist.add(r.getDestNode());
//            Set<Node> visited = new HashSet<>();
//
//            while (!worklist.isEmpty()) {
//                Node n = worklist.pollFirst();
//
//                if(!visited.contains(n))
//                    visited.add(n);
//                else
//                    continue;
//
//                updateVertex(n);
//
//                worklist.addAll(getConnectedOutNodes(n));
//            }
//        }
//        return e;
//
//    }

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
        for(Node n : arg0) {
            removeVertex(n);
        }
        return this.vertexSet().size() < size;
    }

    @Override
    public boolean removeVertex(Node n) {


        LOGGER.debug(toDot());
        if(containsVertex(n)) {
            boolean ok = false;
            for(Node k : vertexSet()) {

                if(k.getId() == n.getId()) {
                    LOGGER.debug("k {}:{}", k.getLabel(), k.getId());
                    LOGGER.debug("n {}:{}", n.getLabel(), n.getId());
                    ok = true;
                }

            }

            assert ok;
            LOGGER.debug("remove V {}:{}", n.getLabel(), n.getId());
            //nodeLookup.remove(n.getLabel());
            return super.removeVertex(n);
        } else {
            return false;
        }
    }

//
//    public void updateVertex(Node n) {
//        //nodeLookup.remove(n.getLabel());
//        String nl = getNewLabelFor(n);
//        n.setLabel(nl);
//        //nodeLookup.put(nl, n);
//    }

    @Override
    public boolean addVertex(Node n) {
        //LOGGER.info("ADD NODE " + n.getLabel());

        // never add a node twice
        //if ((getNodeByLabel(n.getLabel())) != null) {
        //    return false;
        //} else {
            return super.addVertex(n);
           // nodeLookup.put(n.getLabel(),n);
            //return true;
        //}
    }

    protected Node addNode(Node n) {
        addVertex(n);
        return n;
    }

//    protected void buildNodeIdx() {
//        this.nodeLookup.clear();
//        for (Node n : this.vertexSet()) {
//            this.nodeLookup.put(n.getLabel(), n);
//        }
//    }

    //protected Node getNodeByLabel(String label) {
    //    return this.nodeLookup.get(label);
    //}


    protected Collection<Node> getAllVariables() {
        Set<Node> variables = new HashSet<Node>();
        for (Node n : this.vertexSet()) {
            if (n.isOperand() && !n.isLiteral())
                variables.add(n);
        }
        return variables;
    }


    protected void join(NodeKind kind, Node cpoint, ConstraintNetwork othercn) {


        assert (othercn.getStartNode() != null);
        LOGGER.info("Start node is " + othercn.getStartNode().toString());

        for (Node n : othercn.vertexSet()) {
            this.addNode(n);
        }

        for (Edge e : othercn.edgeSet()) {
            this.addConnection(e);
        }

        //Node snode = this.getNodeByLabel(othercn.getStartNode().getLabel());

        assert (this.vertexSet().contains(cpoint));

        this.addOperation(kind, cpoint, othercn.getStartNode());
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

                if(n.getAnnotation().equals("unsat")) {
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

//    protected void debug() {
//        for (Map.Entry<String, Node> e : this.nodeLookup.entrySet()) {
//            LOGGER.info(e.getKey());
//        }
//    }

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
            } else if (n.isOperation() && n.getKind() == NodeKind.EXTERNAL) {
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

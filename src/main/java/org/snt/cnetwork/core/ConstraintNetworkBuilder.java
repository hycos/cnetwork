package org.snt.cnetwork.core;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.consistency.ConsistencyCheckerFactory;
import org.snt.cnetwork.core.domain.NodeDomainFactory;
import org.snt.cnetwork.core.euf.*;
import org.snt.cnetwork.exception.EUFInconsistencyException;

import java.util.*;


public class ConstraintNetworkBuilder implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkBuilder.class);

    private ConstraintNetwork cn;
    //private NodeElemFact nf;
    private EufManager euf;

    public ConstraintNetworkBuilder(ConstraintNetworkBuilder cnb) {
        this.cn = new ConstraintNetwork(cnb.cn);
        //this.nf = new NodeElemFact(this, cnb.nf);
        this.euf = new EufManager(cnb.euf, this);
        assert cnb.vertexSet().size() == this.cn.vertexSet().size();
        this.cn.vertexSet().forEach(v -> euf.attach(v));
    }

    public ConstraintNetworkBuilder() {
        this.cn = new ConstraintNetwork();
        this.euf = new EufManager(this);
    }


    public Node getNodeByLabel(String lbl) {
        LOGGER.debug("get node by label {}", lbl);
        return this.euf.getNodeByLabel(lbl);
    }

    public boolean hasNodeForLabel(String lbl) {
        return this.euf.hasNodeForLabel(lbl);
    }

    public String getLabelForNode(Node n) {
        EquiClass e = euf.getEquiClassForNode(n);
        return e.getCorrespondingElement(n).getLabel()  ;
    }

    public Node addConstraint(NodeKind kind, Node... params) throws
            EUFInconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addConstraint(kind, lst);
    }

    public Node addOperation(NodeKind kind, Node... params) throws
            EUFInconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addOperation(kind, lst);
    }

    public Edge addConnection(Edge e) {
        return cn.addConnection(e);
    }


    public Node addOperand(NodeKind n, String s) {

        LOGGER.debug("add operand {}:{}", n, s);
        Node op = cn.addOperand(n, s);

        try {
            euf.addEquiClass(op);
            return infer(op);
        } catch (EUFInconsistencyException e) {
            LOGGER.error(e.getMessage());
            assert false;
        }
        return null;
    }

    private Node infer(Node n) throws EUFInconsistencyException {
        Node nop = inferEquivalentNode(n);

        LOGGER.debug("NOP {}:{}", nop, nop.getId());
        LOGGER.debug("OP {}:{}", n, n.getId());

        if(!ConsistencyCheckerFactory.INSTANCE.isConsistent(this, nop)) {
            throw new EUFInconsistencyException("malformed operand " + nop
                    .getKind());
        }

        if (n.equals(nop)) {
            euf.attach(nop);
            euf.update(nop);
            return nop;
        } else {
            cn.removeVertex(n);
            return nop;
        }
    }


    public Node addOperation(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {
        Node op = cn.addOperation(kind, params);
        LOGGER.debug("check node {}:{}", op.getLabel(), op.getId());
        return infer(op);
    }

    public Node addConstraint(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {

        Node op = addOperation(kind, params);

        // it seems to be redundant, byt we need this extra node in order to
        // simplify the translation procedure
        Node contraint = addOperation(NodeKind.EQUALS, op, new Operand("true",
                NodeKind.BOOLLIT));
        op.setDomain(NodeDomainFactory.DBTRUE.clone());

        return op;
    }

    public Operation addExtOperation(String identifier, List<Node> params) {
        return cn.addExtOperation(identifier, params);
    }

    public boolean removeAllVertices(Collection<? extends Node> n) {
        return cn.removeAllVertices(n);
    }

    public boolean removeAllEdges(Collection<? extends Edge> e) {
        return cn.removeAllEdges(e);
    }

    public Set<Edge> getAllEdges(Node n1, Node n2) {
        return cn.getAllEdges(n1, n2);
    }


    public ConstraintNetwork getConstraintNetwork() {
        return cn;
    }

    public EufLattice getEufLattice() {
        return euf.getLattice();
    }

    public Edge addConnection(Node src, Node target, EdgeKind kind, int prio) {
        return cn.addConnection(src, target, kind, prio);
    }

    public void addConnections(Set<Edge> edges) {
        cn.addConnections(edges);
    }


    public List<Node> getParametersFor(Node n) {
        return cn.getParametersFor(n);
    }

    public Set<Node> vertexSet() {
        return cn.vertexSet();
    }

    public Set<Edge> edgeSet() {
        return cn.edgeSet();
    }


    public Set<Node> getConnectedInNodes(Node n) {
        return cn.getConnectedInNodes(n);
    }

    public Set<Node> getConnectedOutNodes(Node n) {
        return cn.getConnectedOutNodes(n);
    }

    public void removeEdge(Node src, Node dst) {
        this.cn.removeEdge(src, dst);
    }

    public boolean removeVertex(Node n) {
        LOGGER.debug("remove vertex {}", n.getId());
        return cn.removeVertex(n);
    }

    public boolean containsVertex(Node n) {
        return cn.containsVertex(n);
    }


    public Set<Edge> incomingEdgesOf(Node n) {
        return cn.incomingEdgesOf(n);
    }

    public Set<Edge> outgoingEdgesOf(Node n) {
        return cn.outgoingEdgesOf(n);
    }

    public ConstraintNetworkBuilder clone() {
        return new ConstraintNetworkBuilder(this);
    }

    public void setStartNode(Node n) {
        cn.setStartNode(n);
    }

    public int outDegreeOf(Node n) {
        return cn.outDegreeOf(n);
    }

    public int inDegreeOf(Node n) {
        return cn.inDegreeOf(n);
    }

    public Node registerExtOperation(String bytecodesig, String lbl) {
        return cn.registerExtOperation(bytecodesig, lbl);
    }

    public void join(NodeKind kind, Node cpoint, ConstraintNetworkBuilder
            othercn) {
        cn.join(kind, cpoint, othercn.getConstraintNetwork());
    }


    public ConstraintNetworkBuilder subgraph(Collection<Node> vertices) {
        ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder(this);
        //cb.subgraph(vertices);
        cb.cn = this.cn.subgraph(vertices);
        return cb;
    }

    public Collection<Node> getAllVariables() {
        return cn.getAllVariables();
    }


    public Node relink(Node toReplace, Node replacement) throws EUFInconsistencyException {

        if (!cn.containsVertex(toReplace) && cn.containsVertex(replacement)) {
            //assert false;
            return replacement;
        }

        int id = toReplace.getId();

        assert cn.containsVertex(toReplace);

        Set<Edge> out = cn.outgoingEdgesOf(toReplace);
        Set<Edge> toAdd = new HashSet<>();

        for (Edge e : out) {
            toAdd.add(new Edge(replacement, e.getTarget(), e.getSequence()));
        }

        if (containsVertex(toReplace)) {
            removeVertex(toReplace);
        }

        addConnections(toAdd);

        assert vertexSet().stream().filter(x -> x
                .getId() == id).count() == 0;


        if(!ConsistencyCheckerFactory.INSTANCE.isConsistent(this,
                replacement)) {
            throw new EUFInconsistencyException("malformed operand " + replacement
                    .getId());
        }

        return replacement;
    }


    // @TODO: make inference work properly for operands as well -- we need
    // to ensure that the returned nodes are definetely present in the CN
    public Node inferEquivalentNode(Node n) {

        EquiClass nen = euf.inferActualEquiClassForNode(n);

        EquiClass nn = euf.getEquiClassForNode(n);

        LOGGER.debug("actual {}:{}", nen.getLabel(), nen.getId());
        LOGGER.debug("new {}:{}", nn.getLabel(), nn.getId());

        assert nen != null;
        assert nen != euf.getBottom();
        //assert nen != euf.getTop();

        if (nen.equals(euf.getTop()))
            return n;

        Node emap = nen.getCorrespondingElement(n).getMappedNode();

        // whenever we infer a new fact it will be added to our euf
        // lattice
        if (!emap.equals(n)) {
            EquiClass nemap = nen.union(nn);
            try {
                nn = euf.addEquiClass(nemap);
            } catch (EUFInconsistencyException e) {
                assert false;
            }
            return nn.getCorrespondingElement(n).getMappedNode();
        }
        return emap;
    }



}

package org.snt.cnetwork.core;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.BooleanRange;
import org.snt.cnetwork.core.domain.NodeDomainFactory;
import org.snt.cnetwork.core.euf.*;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;

import java.util.*;
import java.util.stream.Collectors;


public class ConstraintNetworkBuilder
        extends ConstraintNetworkObserver<Node>
        implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkBuilder.class);

    private boolean eufEnabled = false;
    private ConstraintNetwork cn;
    private NodeElemFact nf;
    private EufLattice euf;

    public ConstraintNetworkBuilder(ConstraintNetworkBuilder cnb) {
        eufEnabled = cnb.eufEnabled;
        this.cn = new ConstraintNetwork(cnb.cn);
        if (cnb.eufEnabled) {
            this.nf = new NodeElemFact(this, cnb.nf);
            this.euf = new EufLattice(cnb.euf, this.nf);
            assert cnb.vertexSet().size() == this.cn.vertexSet().size();
        }

        this.cn.vertexSet().forEach(v -> v.attach(this));
    }

    public ConstraintNetworkBuilder(boolean eufEnabled) {
        this.eufEnabled = eufEnabled;
        this.cn = new ConstraintNetwork();
        if (this.eufEnabled) {
            // EUF has to e assigned first here
            this.nf = new NodeElemFact(this);
            this.euf = new EufLattice(nf);
        }
    }


    public ConstraintNetworkBuilder() {
        this(false);
    }

    public NodeElemFact getNodeElementFact() {
        return this.nf;
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

    public Edge addConnection(Edge e, boolean updateLbl) {
        return cn.addConnection(e, updateLbl);
    }


    private List<Node> inferParam(List<Node> param) {
        LOGGER.debug("infer parameters");
        if(eufEnabled) {
            List<Node> ret = new Vector<>();
            for (Node p : param) {
                ret.add(infer(p));
            }
            return ret;
        }
        return param;
    }



    public Node addOperation(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {

        Node op = cn.addOperation(kind, inferParam(params));

        LOGGER.debug("check node {}:{}", op.getLabel(), op.getId());

        Node nop = infer(op);

        LOGGER.debug("NOP {}", nop);
        LOGGER.debug("OP {}", op);

        // there is already an equivalent node present in the cn
        if (!op.equals(nop) && !op.getLabel().equals(nop.getLabel())) {
            // we can drop the vertex to be added
            cn.removeVertex(op);

            return nop;
        }
        attach(op);
        update(op);
        return op;
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

    public Node getNodeByLabel(String lbl) {

        if (eufEnabled) {
            if (!nf.getNodeCache().containsKey(lbl)) {
                LOGGER.debug("null");
                return null;
            }

            EquiClass ec = nf.getNodeCache().getValueByKey(lbl);
            Set<EquiClass> snen = euf.inferEquiClassFor(ec);

            LOGGER.debug("ieq {}", snen);

            assert snen.size() == 1;

            EquiClass nen = snen.iterator().next();

            if (nen == null || nen == euf.getBottom() || nen == euf.getTop()) {
                return null;
            }

            LOGGER.debug("equivalent class {}", nen.getDotLabel());
            assert nen.isSingleton();

            Element<Node> e = nen.getElements().iterator().next();
            Node emap = e.getMappedNode();

            LOGGER.debug("mapped element is {}", emap.getLabel());

            return emap;
        } else {
            return cn.getNodeByLabel(lbl);
        }
    }

    public Node getNodeById(int id) {
        // @TODO:Julian might have to change that
        return cn.getNodeById(id);
    }

    public ConstraintNetwork getConstraintNetwork() {
        return cn;
    }

    public EufLattice getEufLattice() {
        return this.euf;
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
        this.cn.removeEdge(src,dst);
    }

    public boolean removeVertex(Node n) {
        LOGGER.debug("remove vertex {}", n.getId());
        return cn.removeVertex(n);
    }

    public boolean containsVertex(Node n) {
        return cn.containsVertex(n);
    }

    public Node addOperand(NodeKind kind, String label) {
        Node n = cn.addOperand(kind, label);
        if(eufEnabled)
            nf.createEquiClass(n);
        return n;
    }

    public Set<Edge> incomingEdgesOf(Node n) {
        return incomingEdgesOf(n);
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

    private boolean isRedundantPar(Node p, boolean val) {
        if (!p.isBoolean())
            return false;

        BooleanRange br = (BooleanRange) p.getRange();
        return (br.isAlwaysTrue() && val) || (br.isAlwaysFalse() && !val);
    }

    private Node[] getParList(Collection<Node> n, boolean val) {
        List<Node> r = n.stream().filter(x -> !isRedundantPar(x, val)).collect
                (Collectors
                        .toList());
        return r.toArray(new Node[r.size()]);
    }

    private boolean hasRedundantPars(Collection<Node> ps, boolean val) {
        return ps.stream().filter(x -> isRedundantPar(x, val)).count() > 0;
    }

    private void attach(Node n) {
        if (eufEnabled) {
            LOGGER.debug("Attach listener to {}", n.getLabel());
            n.attach(this);
        }
    }

    private Node infer(Node n) {

        if(eufEnabled) {
            Node nn = inferEquivalentNode(n);
            // is already present as nn1
            if (!nn.equals(n)) {

                LOGGER.debug("inferred \nt:{}:{}\nf:{}:{}", nn.getLabel(),nn
                        .getId(), n
                        .getLabel(),n.getId());
                return nn;
            }
        }
        // n is the first of its kind
        return n;
    }


    public Node relink(Node toReplace, Node replacement) {
        int id = toReplace.getId();

        assert cn.containsVertex(toReplace);

        Set<Edge> out = cn.outgoingEdgesOf(toReplace);
        Set<Edge> toAdd = new HashSet<>();

        for(Edge e : out) {
            toAdd.add(new Edge(replacement, e.getTarget(), e.getSequence()));
        }

        if(containsVertex(toReplace)) {
            removeVertex(toReplace);
        }

        addConnections(toAdd);

        assert vertexSet().stream().filter(x -> x
                .getId() == id).count() == 0;

        return replacement;
    }


    private Node inferEquivalentNode(Node n) {

        LOGGER.debug("infer equivalent node {}:{}", n.getLabel(), n.getId());
        if (n.isOperand()) {
            return n;
        } else {
            // create temporary equi class
            nf.createEquiClass(n);
            EquiClass en = nf.getNodeCache().getValueByKey(n.getLabel());
            Set<EquiClass> snen = euf.inferEquiClassFor(en);

            LOGGER.debug("ieq {}", snen);

            //LOGGER.debug(getEufLattice().toDot());

            assert snen.size() == 1;

            EquiClass nen = snen.iterator().next();

            if (nen == null || nen == euf.getBottom() || nen == euf.getTop()) {
                return n;
            }

            LOGGER.debug("equivalent class {}", nen.getDotLabel());
            assert nen.isSingleton();

            Element<Node> e = nen.getElements().iterator().next();
            Node emap = e.getMappedNode();

            LOGGER.debug("mapped element is {}:{}", emap.getLabel(), emap.getId());

            return emap;
        }
    }

    public void updateVertex(Node n) {
        cn.updateVertex(n);
    }

    @Override
    public void update(Node n) throws EUFInconsistencyException {
        LOGGER.debug(">> update {}", n.getDotLabel());

        if (eufEnabled) {
            if (n.isNumeric() && n.getRange().isSingleton
                    () && !n.isLiteral()) {


                LOGGER.debug("RAN " + n.getLabel() + " " + n.getRange()
                        .toString
                                ());
                nf.createEquiClass(n);

                EquiClass eq = null;
                try {
                    eq = nf.getEquiClassFor(n).clone();
                } catch (MissingItemException e) {
                    assert false;
                }

                eq.addElement(new SingletonElement(n, n.getRange().getMin()
                        .getEndpoint().toString()));

                LOGGER.debug("new eq {}", eq.toString());

                euf.addEquiClass(eq);

            } else if (n.isString() && n.getAutomaton().isSingleton() && !n.isLiteral()) {

                nf.createEquiClass(n);

                EquiClass eq = null;
                try {
                    eq = nf.getEquiClassFor(n).clone();
                } catch (MissingItemException e) {
                    assert false;
                }

                eq.addElement(new SingletonElement(n, "\"" + n.getAutomaton
                        ().getShortestExample() + "\""));

                LOGGER.debug("new eq {}", eq.toString());

                euf.addEquiClass(eq);
            } else if (n.isBoolean() && !n.isLiteral()) {

                if (n.getKind().isInequality()) {
                    if (((BooleanRange) n.getRange()).isAlwaysFalse()) {
                        List<Node> pars = cn.getParametersFor(n);
                        assert pars.size() == 2;
                        euf.addEquiClass(getParList(pars, false));
                    }
                    if (((BooleanRange) n.getRange()).isAlwaysTrue()) {
                        List<Node> pars = cn.getParametersFor(n);
                        assert pars.size() == 2;
                        if (!hasRedundantPars(pars, true))
                            euf.addInequialityConstraint(pars.get(0), pars.get(1));
                    }
                } else if (n.getKind().isEquality()) {
                    LOGGER.debug("n {}", n.getDotLabel());
                    assert n.getRange() instanceof BooleanRange;
                    if (((BooleanRange) n.getRange()).isAlwaysTrue()) {
                        List<Node> pars = cn.getParametersFor(n);
                        assert pars.size() == 2;
                        euf.addEquiClass(getParList(pars, true));
                    }
                    if (((BooleanRange) n.getRange()).isAlwaysFalse()) {
                        List<Node> pars = cn.getParametersFor(n);
                        assert pars.size() == 2;
                        if (!hasRedundantPars(pars, false))
                            euf.addInequialityConstraint(pars.get(0), pars.get(1));
                    }
                }
            } else if (!n.isBoolean() && n.isOperation()) {
                euf.addEquiClass(n);
            }
        }

    }
}

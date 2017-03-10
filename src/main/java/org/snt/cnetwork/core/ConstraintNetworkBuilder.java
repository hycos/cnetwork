package org.snt.cnetwork.core;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.BooleanRange;
import org.snt.cnetwork.core.mergelattice.*;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;


public class ConstraintNetworkBuilder
        extends ConstraintNetworkObserver<Node>
        implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkBuilder.class);

    private ConstraintNetwork cn;
    private NodeElemFact nf;
    private EufLattice<Node> euf;

    boolean eufEnabled = false;

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
            this.euf = new EufLattice<>(nf);
        }
    }


    public NodeElemFact getNodeElementFact() {
        return this.nf;
    }

    public ConstraintNetworkBuilder() {
        this(false);
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


    public Node addOperation(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {
        Node op = cn.addOperation(kind, false, params);
        op = infer(op);
        attach(op);
        update(op);
        return op;
    }

    public Node addConstraint(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {
        Node op = cn.addOperation(kind, true, params);
        LOGGER.debug(">> add constraint {}", op);
        op = infer(op);
        attach(op);
        update(op);
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

    public Node getNodeByLabel(String lbl) {
        return cn.getNodeByLabel(lbl);
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

    public boolean removeVertex(Node n) {
        return cn.removeVertex(n);
    }

    public boolean containsVertex(Node n) {
        return cn.containsVertex(n);
    }

    public Operand addOperand(NodeKind kind, String label) {
        return cn.addOperand(kind, label);
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

    public Node getNodeById(int id) {
        return cn.getNodeById(id);
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

    public Node infer(Node n) {

        Node nn = inferEquivalentNode(n);

        // is already present as nn
        if(!nn.equals(n)) {

            LOGGER.debug("inferred {}", nn.getDotLabel());
            return nn;
        }

        // n is the first of its kind
        return n;
    }



    private Node inferEquivalentNode(Node n) {
        if(n.isOperand()) {
            return n;
        } else {
            // create temporary equi class
            nf.createEquiClass(n);
            EquiClass en = nf.getNodeCache().getValueByKey(n);
            EquiClass nen = euf.inferEquiClassFor(en);

            if(nen == null || nen == euf.getBottom() || nen == euf.getTop()) {
                return n;
            }

            LOGGER.debug("equivalent class {}", nen.getDotLabel());
            assert nen.isSingleton();

            Element<Node> e =  nen.getElements().iterator().next();
            return e.getMappedElement();
        }
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

                eq.addElement(new SingletonElement(n, "\"" + n .getAutomaton
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
            } else if(!n.isBoolean() && n.isOperation()) {
                euf.addEquiClass(n);
            }
        }

    }
}

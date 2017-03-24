package org.snt.cnetwork.core;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.BooleanRange;
import org.snt.cnetwork.core.domain.NodeDomainFactory;
import org.snt.cnetwork.core.euf.EquiClass;
import org.snt.cnetwork.core.euf.EufLattice;
import org.snt.cnetwork.core.euf.NodeElemFact;
import org.snt.cnetwork.core.euf.SingletonElement;
import org.snt.cnetwork.exception.EUFInconsistencyException;

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

    public ConstraintNetworkBuilder() {
        this.eufEnabled = true;
        this.cn = new ConstraintNetwork();
        //if (this.eufEnabled) {
        // EUF has to e assigned first here
        this.nf = new NodeElemFact(this);
        this.euf = new EufLattice(nf);
        //}
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

    public Edge addConnection(Edge e) {
        return cn.addConnection(e);
    }


    private List<Node> inferParam(List<Node> param) {
        LOGGER.debug("infer parameters");
        List<Node> ret = new Vector<>();
        for (Node p : param) {
            ret.add(inferEquivalentNode(p));
        }
        return ret;

    }


    public Node addOperation(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {


        Node op = cn.addOperation(kind, params);

        LOGGER.debug("check node {}:{}", op.getLabel(), op.getId());

        Node nop = inferEquivalentNode(op);

        LOGGER.debug("NOP {}:{}", nop, nop.getId());
        LOGGER.debug("OP {}:{}", op, op.getId());

        if(op.equals(nop)) {
            attach(nop);
            update(nop);
            return nop;
        } else {
            cn.removeVertex(op);
            return nop;
        }
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

    /**
     * infer the actual equiclass to which a node belongs to
     *
     * @param n
     * @return
     */
    private EquiClass inferActualEquiClassForNode(Node n) {
        nf.createEquiClass(n);
        EquiClass ec = nf.getEquiClassFor(n);
        Set<EquiClass> snen = euf.inferEquiClassFor(ec);
        LOGGER.debug("ieq {}", snen);
        assert snen.size() == 1;
        EquiClass nen = snen.iterator().next();
        return nen;
    }

    public String getLabelForNode(Node n) {
        EquiClass nen = inferActualEquiClassForNode(n);
        if (nen == null || nen == euf.getBottom() || nen == euf.getTop()) {
            return null;
        }
        return nen.getCorrespondingElement(n).getLabel();
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
        this.cn.removeEdge(src, dst);
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
        //if(eufEnabled)
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


    public Node relink(Node toReplace, Node replacement) {

        if (!cn.containsVertex(toReplace) && cn.containsVertex(replacement))
            return replacement;

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

        return replacement;
    }


    // @TODO: make inference work properly for operands as well -- we need
    // to ensure that the returned nodes are definetely present in the CN
    public Node inferEquivalentNode(Node n) {

        EquiClass nen = inferActualEquiClassForNode(n);

        EquiClass nn = nf.getEquiClassFor(n);

        LOGGER.debug("actual {}:{}", nen.getDotLabel(), nen.getId());
        LOGGER.debug("new {}:{}", nn.getDotLabel(), nn.getId());

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


    @Override
    public void update(Node n) throws EUFInconsistencyException {
        LOGGER.debug(">> update {}", n.getDotLabel());

        if (n.isNumeric() && n.getRange().isSingleton
                () && !n.isLiteral()) {


            LOGGER.debug("RAN " + n.getLabel() + " " + n.getRange()
                    .toString
                            ());
            nf.createEquiClass(n);

            EquiClass eq = null;

            eq = nf.getEquiClassFor(n).clone();


            eq.addElement(new SingletonElement(n, n.getRange().getMin()
                    .getEndpoint().toString()));

            LOGGER.debug("new eq {}", eq.toString());

            euf.addEquiClass(eq);

        } else if (n.isString() && n.getAutomaton().isSingleton() && !n.isLiteral()) {

            nf.createEquiClass(n);

            EquiClass eq = null;

            eq = nf.getEquiClassFor(n).clone();


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

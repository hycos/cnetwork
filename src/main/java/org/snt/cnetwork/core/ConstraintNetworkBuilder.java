package org.snt.cnetwork.core;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.BooleanRange;
import org.snt.cnetwork.core.mergelattice.MergeLattice;
import org.snt.cnetwork.core.mergelattice.NodeElemFact;
import org.snt.cnetwork.exception.EUFInconsistencyException;

import java.util.*;
import java.util.stream.Collectors;


public class ConstraintNetworkBuilder
        extends ConstraintNetworkObserver<Node>
        implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkBuilder.class);

    private ConstraintNetwork cn = new ConstraintNetwork();
    private NodeElemFact nf = new NodeElemFact(this);
    private MergeLattice<Node> euf = new MergeLattice<>(nf);

    boolean eufEnabled = false;

    public ConstraintNetworkBuilder(ConstraintNetworkBuilder cnb) {
        eufEnabled = cnb.eufEnabled;
        if(cnb.eufEnabled) {
            cn = new ConstraintNetwork(cnb.cn);
            this.nf = new NodeElemFact(this,cnb.nf);
            this.euf = new MergeLattice(this.nf);
        }
    }

    public ConstraintNetworkBuilder(boolean eufEnabled) {
        this.eufEnabled = eufEnabled;
    }

    public ConstraintNetworkBuilder(){
        this(false);
    }

    public Operation addConstraint(NodeKind kind, Node... params) throws
            EUFInconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addConstraint(kind, lst);
    }

    public Operation addOperation(NodeKind kind, Node... params) throws
            EUFInconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addOperation(kind, lst);
    }

    public Edge addConnection(Edge e, boolean updateLbl) {
        return cn.addConnection(e, updateLbl);
    }


    public Operation addOperation(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {
        Operation op = cn.addOperation(kind, false, params);
        //if(eufEnabled)
        //    op.attach(this);
        update(op);
        return op;
    }

    public Operation addConstraint(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {
        Operation op = cn.addOperation(kind, true, params);
        //if(eufEnabled)
        //    op.attach(this);
        update(op);
        return op;
    }

    public Node addNode(Node n) throws EUFInconsistencyException {
        Node nn = cn.addNode(n);
        //if(eufEnabled)
        //    nn.attach(this);
        update(n);
        return nn;
    }

    public Operation addExtOperation(String identifier, List<Node> params) {
        return cn.addExtOperation(identifier,params);
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

    public MergeLattice getEufLattice() {
        return this.euf;
    }

    public Edge addConnection(Node src, Node target, EdgeKind kind, int prio) {
        return cn.addConnection(src,target,kind,prio);
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

    public boolean removeVertex(Node n){
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
        cn.join(kind,cpoint,othercn.getConstraintNetwork());
    }

    public Node getNodeById(int id) {
        return cn.getNodeById(id);
    }

    public ConstraintNetworkBuilder subgraph(Collection<Node> vertices) {
        ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder(this);
        cb.subgraph(vertices);
        return cb;
    }

    public Collection<Node> getAllVariables() {
        return cn.getAllVariables();
    }

    private boolean isRedundantPar(Node p, boolean val) {
        if (!p.isBoolean())
            return false;

        BooleanRange br = (BooleanRange)p.getRange();
        return (br.isAlwaysTrue() && val) || (br.isAlwaysFalse() && !val);
    }

    private Node [] getParList(Collection<Node> n, boolean val) {
        List<Node> r = n.stream().filter(x -> !isRedundantPar(x, val)).collect
                (Collectors
                .toList());
        return r.toArray(new Node[r.size()]);
    }

    private boolean hasRedundantPars(Collection<Node> ps, boolean val) {
        return ps.stream().filter(x -> isRedundantPar(x, val)).count() > 0;
    }

    @Override
    public void update(Node n) throws EUFInconsistencyException {


        if(eufEnabled) {
            if (n.getKind().isEquality()) {
                assert n.getRange() instanceof BooleanRange;
                if (((BooleanRange) n.getRange()).isAlwaysTrue()) {
                    List<Node> pars = cn.getParametersFor(n);
                    assert pars.size() == 2;
                    euf.addEquiClass(getParList(pars, true));
                }
                if (((BooleanRange) n.getRange()).isAlwaysFalse()) {
                    List<Node> pars = cn.getParametersFor(n);
                    assert pars.size() == 2;
                    if(!hasRedundantPars(pars, false));
                        euf.addInequialityConstraint(pars.get(0), pars.get(1));
                }
            } else if (n.getKind().isInequality()) {
                if (((BooleanRange) n.getRange()).isAlwaysFalse()) {
                    List<Node> pars = cn.getParametersFor(n);
                    assert pars.size() == 2;
                    euf.addEquiClass(getParList(pars,false));
                }
                if (((BooleanRange) n.getRange()).isAlwaysTrue()) {
                    List<Node> pars = cn.getParametersFor(n);
                    assert pars.size() == 2;
                    if(!hasRedundantPars(pars, true));
                        euf.addInequialityConstraint(pars.get(0), pars.get(1));
                }
            }
        }
    }
}

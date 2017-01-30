package org.snt.cnetwork.core;

import org.snt.cnetwork.core.domain.BooleanRange;
import org.snt.cnetwork.core.mergelattice.MergeLattice;
import org.snt.cnetwork.core.mergelattice.NodeElemFact;
import org.snt.cnetwork.exception.EUFInconsistencyException;

import java.util.Arrays;
import java.util.List;
import java.util.Set;


public class ConstraintNetworkBuilder extends ConstraintNetworkObserver<Node> {

    private ConstraintNetwork cn = new ConstraintNetwork();
    private NodeElemFact nf = new NodeElemFact(cn);
    private MergeLattice<Node> euf = new MergeLattice<Node>(nf);

    boolean eufEnabled = false;

    public ConstraintNetworkBuilder(ConstraintNetworkBuilder cnb) {
        eufEnabled = cnb.eufEnabled;
        if(cnb.eufEnabled) {
            this.cn = new ConstraintNetwork(cnb.cn);
            this.nf = new NodeElemFact(this.cn,cnb.nf);
            this.euf = new MergeLattice(new NodeElemFact(cnb.cn));
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
        Operation n = cn.addConstraint(kind, lst);

        if (kind.isEquality() && eufEnabled) {
            euf.addEquiClass(params);
        } else if (kind.isInequality() && eufEnabled) {
            assert params.length == 2;
            euf.addInequialityConstraint(params[0], params[1]);
        }

        return n;
    }

    public Operation addOperation(NodeKind kind, Node... params) {
        List<Node> lst = Arrays.asList(params);
        Operation op = cn.addOperation(kind, lst);
        if(eufEnabled)
            op.attach(this);
        return op;
    }

    public Operation addOperation(NodeKind kind, List<Node> params) {
        Operation op = cn.addOperation(kind, false, params);
        if(eufEnabled)
            op.attach(this);
        return op;
    }

    public Operation addConstraint(NodeKind kind, List<Node> params) {
        Operation op = cn.addOperation(kind, true, params);
        op.attach(this);
        return op;
    }

    public Node addNode(Node n) {
        return cn.addNode(n);
    }

    public Node getNodeByLabel(String lbl) {
        return cn.getNodeByLabel(lbl);
    }

    public ConstraintNetwork getConstraintNetwork() {
        return this.cn;
    }

    public MergeLattice getEufLattice() {
        return this.euf;
    }

    public Edge addConnection(Node src, Node target, EdgeKind kind, int prio) {
        return this.cn.addConnection(src,target,kind,prio);
    }

    public void addConnections(Set<Edge> edges) {
       this.cn.addConnections(edges);
    }


    @Override
    public void update(Node n) throws EUFInconsistencyException {
        if(n.getKind().isEquality()) {
            assert n.getRange() instanceof BooleanRange;
            if(((BooleanRange) n.getRange()).isAlwaysTrue()) {
                List<Node> pars = cn.getParametersFor(n);
                assert pars.size() == 2;
                euf.addEquiClass(pars.get(0), pars.get(1));
            }
            if(((BooleanRange) n.getRange()).isAlwaysFalse()) {
                List<Node> pars = cn.getParametersFor(n);
                assert pars.size() == 2;
                euf.addInequialityConstraint(pars.get(0), pars.get(1));
            }
        } else if (n.getKind().isInequality()) {
            if(((BooleanRange) n.getRange()).isAlwaysFalse()) {
                List<Node> pars = cn.getParametersFor(n);
                assert pars.size() == 2;
                euf.addEquiClass(pars.get(0), pars.get(1));
            }
            if(((BooleanRange) n.getRange()).isAlwaysTrue()) {
                List<Node> pars = cn.getParametersFor(n);
                assert pars.size() == 2;
                euf.addInequialityConstraint(pars.get(0), pars.get(1));
            }
        }
    }
}

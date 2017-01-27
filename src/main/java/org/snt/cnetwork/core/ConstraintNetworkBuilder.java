package org.snt.cnetwork.core;

import org.snt.cnetwork.core.mergelattice.MergeLattice;
import org.snt.cnetwork.core.mergelattice.NodeElemFact;
import org.snt.cnetwork.exception.EUFInconsistencyException;

import java.util.Arrays;
import java.util.List;


public class ConstraintNetworkBuilder {

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
        return cn.addOperation(kind, lst);
    }

    public ConstraintNetwork getConstraintNetwork() {
        return this.cn;
    }

    public MergeLattice getEufLattice() {
        return this.euf;
    }

}

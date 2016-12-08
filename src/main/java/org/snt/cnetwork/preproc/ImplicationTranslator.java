package org.snt.cnetwork.preproc;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Edge;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;

import java.util.List;

public class ImplicationTranslator implements Converter {

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkPreprocessor.class);


    @Override
    public NodeKind getKind() {
        return NodeKind.IMPLIES;
    }

    @Override
    public void translate(ConstraintNetwork cn, Node n) {
        assert n.getKind() == NodeKind.IMPLIES;
        List<Node> pars = cn.getParametersFor(n);
        assert pars.size() == 2;
        Node par0 = pars.get(0);
        Node par1 = pars.get(1);
        Node npar0 = cn.addOperation(NodeKind.NOT, par0);
        Node or = cn.addOperation(NodeKind.OR, npar0, par1);
        or.setDomain(n.getDomain().clone());
        for(Edge e : cn.outgoingEdgesOf(n)) {
            Edge ne = new Edge(or, e.getDestNode(),e.getSequence());
            cn.addConnection(ne);
        }
        cn.removeVertex(n);
    }

}

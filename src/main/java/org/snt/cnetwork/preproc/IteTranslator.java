package org.snt.cnetwork.preproc;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Edge;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;

import java.util.List;

public class IteTranslator implements Converter {

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkPreprocessor.class);

    @Override
    public NodeKind getKind() {
        return NodeKind.ITE;
    }

    @Override
    public void translate(ConstraintNetwork cn, Node n) {
        assert n.getKind() == NodeKind.ITE;

        List<Node> pars = cn.getParametersFor(n);

        assert pars.size() == 3;

        Node par0 = pars.get(0);
        Node par1 = pars.get(1);
        Node par2 = pars.get(2);

        Node impl0 = cn.addOperation(NodeKind.IMPLIES, par0, par1);
        Node npar0 = cn.addOperation(NodeKind.NOT, par0);
        Node impl1 = cn.addOperation(NodeKind.IMPLIES, npar0, par2);
        Node or = cn.addOperation(NodeKind.OR, impl0, impl1);
        or.setDomain(n.getDomain().clone());

        for(Edge e : cn.outgoingEdgesOf(n)) {
            cn.addEdge(e.getDestNode(),or);
        }
        cn.removeVertex(n);
    }

}

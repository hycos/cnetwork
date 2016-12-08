package org.snt.cnetwork.preproc;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.core.domain.NodeDomain;

import java.util.List;

public class ImplicationTranslator implements Converter {

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkPreprocessor.class);


    DisjunctionTranslator dis = new DisjunctionTranslator();

    @Override
    public void translate(ConstraintNetwork cn, Node n) {
        List<Node> pars = cn.getParametersFor(n);
        assert pars.size() == 2;
        Node par0 = pars.get(0);
        NodeDomain p0d = par0.getDomain();
        NodeDomain dd = n.getDomain();
        par0.setDomain(p0d.negate());
        n.setDomain(dd.negate());
        // translate the whole thing into and AND afterwards
        n.setKind(NodeKind.OR);
        dis.translate(cn, n);
    }

    @Override
    public boolean match(ConstraintNetwork cn, Node n) {
        return n.getKind() == NodeKind.IMPLIES;
    }
}

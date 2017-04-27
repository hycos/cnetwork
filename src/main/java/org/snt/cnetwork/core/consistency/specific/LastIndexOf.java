package org.snt.cnetwork.core.consistency.specific;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.core.graph.NodeKind;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;

import java.util.List;


public class LastIndexOf extends ConsistencyChecker {

    final static Logger LOGGER = LoggerFactory.getLogger(LastIndexOf.class);

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        LOGGER.debug("K {}", n.getKind());

        if(params.size() != 2)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);

        return (n.getKind() == NodeKind.LASTINDEXOF &&
                n.getKind().isNumeric() &&
                params != null &&
                !par0.isNumeric() &&
                !par1.isNumeric());
    }
}

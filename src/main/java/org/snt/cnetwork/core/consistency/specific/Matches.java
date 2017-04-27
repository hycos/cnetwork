package org.snt.cnetwork.core.consistency.specific;

import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.core.graph.NodeKind;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;

import java.util.List;


public class Matches extends ConsistencyChecker {
    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {
        List<Node> params = cb.getParametersFor(n);

        if(params.size() != 2)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);


        return par0.getKind().isString() && par1.getKind().isString() &&
                n.getKind().isBoolean() && n.getKind() == NodeKind.MATCHES;
    }
}

package org.snt.cnetwork.core.consistency.specific;

import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.core.graph.NodeKind;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;

import java.util.List;

/**
 * Created by julian on 26/03/2017.
 */
public class Len extends ConsistencyChecker {
    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        if(params.size() != 1)
            return false;

        Node par0 = params.get(0);

        return par0.isString() && n.isNumeric() && n.getKind() == NodeKind.LEN;


    }
}

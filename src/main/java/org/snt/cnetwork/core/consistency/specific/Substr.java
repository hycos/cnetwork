package org.snt.cnetwork.core.consistency.specific;

import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.core.graph.NodeKind;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;

import java.util.List;

/**
 * Created by julian on 26/03/2017.
 */
public class Substr extends ConsistencyChecker {
    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        if(params.size() != 3)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);
        Node par2 = params.get(2);

        return (n.getKind() == NodeKind.SUBSTR &&
                params != null &&
                params.size() == 3 &&
                !par0.isNumeric() &&
                par1.isNumeric() &&
                par2.isNumeric());

    }
}

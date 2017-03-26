package org.snt.cnetwork.core.consistency.general;

import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;

import java.util.List;

/**
 * Created by julian on 26/03/2017.
 */
public class BooleanUnaryOp  implements ConsistencyChecker {

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        if(params.size() != 1)
            return false;

        Node par0 = params.get(0);


        return par0.getKind().isBoolean() &&
                n.getKind().isBoolean();
    }
}

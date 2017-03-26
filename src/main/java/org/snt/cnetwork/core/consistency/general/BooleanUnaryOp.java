package org.snt.cnetwork.core.consistency.general;

import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;

/**
 * Created by julian on 26/03/2017.
 */
public class BooleanUnaryOp extends ConsistencyChecker {

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        return checkNary(cb, n, 1, x ->x.getKind().isBoolean(), p -> p
                .getKind()
                .isBoolean());
    }
}

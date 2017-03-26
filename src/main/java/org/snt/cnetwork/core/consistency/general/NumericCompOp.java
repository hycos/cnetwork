package org.snt.cnetwork.core.consistency.general;

import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;


public class NumericCompOp extends ConsistencyChecker {

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {
        return checkNary(cb, n, 2, x ->x.getKind().isNumeric(), p -> p
                .getKind()
                .isNumeric());
    }
}

package org.snt.cnetwork.core.consistency.general;

import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;


public class NumericBinaryOp extends ConsistencyChecker {


    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {
        return checkNary(cb, n, 2, x ->x.getKind().isNumeric(), p -> p
                .getKind()
                .isNumeric());
    }
}

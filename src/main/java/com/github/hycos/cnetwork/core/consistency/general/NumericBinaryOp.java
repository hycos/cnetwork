package com.github.hycos.cnetwork.core.consistency.general;

import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;


public class NumericBinaryOp extends ConsistencyChecker {


    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {
        return checkNary(cb, n, 2, x ->x.getKind().isNumeric(), p -> p
                .getKind()
                .isNumeric());
    }
}

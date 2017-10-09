package com.github.hycos.cnetwork.core.consistency.general;

import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;

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

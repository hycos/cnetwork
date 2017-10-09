package com.github.hycos.cnetwork.core.consistency.general;

import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;

/**
 * Created by julian on 26/03/2017.
 */
public class StringBinaryOp  extends ConsistencyChecker {
    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        return checkNary(cb, n, 2, x ->x.getKind().isString(), p -> p
                .getKind()
                .isString());
    }
}

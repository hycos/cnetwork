package org.snt.cnetwork.core.consistency.general;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;


public class BooleanBinaryOp extends ConsistencyChecker {

    final static Logger LOGGER = LoggerFactory.getLogger(BooleanBinaryOp.class);

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        return checkNary(cb, n, 2, x ->x.getKind().isBoolean(), p -> p
                .getKind()
                .isBoolean());

    }
}

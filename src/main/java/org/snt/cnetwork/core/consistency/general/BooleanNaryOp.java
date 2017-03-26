package org.snt.cnetwork.core.consistency.general;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;


public class BooleanNaryOp extends ConsistencyChecker {

    final static Logger LOGGER = LoggerFactory.getLogger(BooleanNaryOp.class);

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        return checkNary(cb, n, -1, x ->x.getKind().isBoolean(), p -> p
                .getKind()
                .isBoolean());

    }
}

package com.github.hycos.cnetwork.core.consistency.general;

import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class BooleanBinaryOp extends ConsistencyChecker {

    final static Logger LOGGER = LoggerFactory.getLogger(BooleanBinaryOp.class);

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        return checkNary(cb, n, 2, x ->x.getKind().isBoolean(), p -> p
                .getKind()
                .isBoolean());

    }
}

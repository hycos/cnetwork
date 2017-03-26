package org.snt.cnetwork.core.consistency;

import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;

/**
 * An interface for checking consistency of operations and
 * for type inference
 */
public interface ConsistencyChecker {
    boolean check(ConstraintNetworkBuilder cb, Node n);
}

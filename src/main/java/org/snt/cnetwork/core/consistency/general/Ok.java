package org.snt.cnetwork.core.consistency.general;

import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.consistency.ConsistencyChecker;

/**
 * Created by julian on 26/03/2017.
 */
public class Ok extends ConsistencyChecker {
    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {
        return true;
    }
}

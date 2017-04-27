package org.snt.cnetwork.core.consistency;

import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.Node;

import java.util.List;
import java.util.function.Predicate;

/**
 * An interface for checking consistency of operations and
 * for type inference
 */
public abstract class ConsistencyChecker {
    public abstract boolean check(ConstraintNetworkBuilder cb, Node n);

    protected boolean checkNary(ConstraintNetworkBuilder cb,
                                Node op,
                                int arity,
                                Predicate<Node> paramPredicate,
                                Predicate<Node> opPredicate) {

        List<Node> params = cb.getParametersFor(op);

        if(arity > 0) {
            if(params.size() != arity)
                return false;
        }

        return params.stream().filter(paramPredicate).count() == params.size
                () && opPredicate.test(op);

    }

    protected boolean checkNaryPlus2(ConstraintNetworkBuilder cb,
                                Node op,
                                Predicate<Node> paramPredicate,
                                Predicate<Node> opPredicate) {

        List<Node> params = cb.getParametersFor(op);

        if(params.size() < 2) {
            return false;
        }


        return params.stream().filter(paramPredicate).count() == params.size
                () && opPredicate.test(op);

    }

}

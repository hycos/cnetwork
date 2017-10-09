package com.github.hycos.cnetwork.core.consistency.specific;

import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.graph.Node;

import java.util.List;

/**
 * Created by julian on 26/03/2017.
 */
public class Len extends ConsistencyChecker {
    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        if(params.size() != 1)
            return false;

        Node par0 = params.get(0);

        return par0.isString() && n.isNumeric() && n.getKind() == NodeKind.LEN;


    }
}

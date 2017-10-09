package com.github.hycos.cnetwork.core.consistency.specific;

import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;

import java.util.List;

/**
 * Created by julian on 26/03/2017.
 */
public class Substr extends ConsistencyChecker {
    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        if(params.size() != 3)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);
        Node par2 = params.get(2);

        return (n.getKind() == NodeKind.SUBSTR &&
                params != null &&
                params.size() == 3 &&
                !par0.isNumeric() &&
                par1.isNumeric() &&
                par2.isNumeric());

    }
}

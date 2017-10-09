package com.github.hycos.cnetwork.core.consistency.specific;

import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;

import java.util.List;

/**
 * Created by julian on 26/03/2017.
 */
public class Ite extends ConsistencyChecker {
    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {
        List<Node> params = cb.getParametersFor(n);

        if(params.size() != 3)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);
        Node par2 = params.get(2);


        return par0.isBoolean() && par1.isBoolean() && par2.isBoolean() && n
                .isBoolean() && n.getKind() == NodeKind.ITE;



    }
}

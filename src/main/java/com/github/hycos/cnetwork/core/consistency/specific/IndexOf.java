package com.github.hycos.cnetwork.core.consistency.specific;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.EdgeKind;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

import java.util.List;


public class IndexOf extends ConsistencyChecker {

    final static Logger LOGGER = LoggerFactory.getLogger(IndexOf.class);

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        if(params.size() < 2 || params.size() > 4)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);
        Node par2;

        if(params.size() == 3) {
            par2 = params.get(2);
        } else {
            par2 = cb.addOperand(NodeKind.NUMLIT, "0");
            try {
                cb.addConnection(par2, n, EdgeKind.PAR_IN, 2);
            } catch (EUFInconsistencyException e) {
                assert false;
            }
        }

        LOGGER.debug("idx {}:{}:{}", par0.getKind(),par1.getKind(),par2.getKind());

        return (n.getKind() == NodeKind.INDEXOF &&
                n.getKind().isNumeric() &&
                params != null &&
                !par0.isNumeric() &&
                !par1.isNumeric()) &&
                par2.isNumeric();
    }
}

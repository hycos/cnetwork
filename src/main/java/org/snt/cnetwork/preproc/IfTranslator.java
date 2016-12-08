package org.snt.cnetwork.preproc;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;

import java.util.List;

public class IfTranslator implements Converter {

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkPreprocessor.class);


    DisjunctionTranslator dis = new DisjunctionTranslator();

    @Override
    public void translate(ConstraintNetwork cn, Node n) {
        List<Node> params = cn.getParametersFor(n);

        assert params.size() == 2;

        Node cond = params.get(0);
        Node path0 = params.get(1);
        Node path1= params.get(2);

        assert cond.isBoolean();
        assert path0.getDomain().equals(path1.getDomain());





    }

    @Override
    public boolean match(ConstraintNetwork cn, Node n) {
        return n.getKind() == NodeKind.ITE;
    }
}

package org.snt.cnetwork.preproc;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;

import java.util.HashSet;
import java.util.Set;

public enum CnetworkPreprocessor {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkPreprocessor.class);

    final static DisjunctionTranslator dtrans = new DisjunctionTranslator();
    final static ImplicationTranslator impl = new ImplicationTranslator();



    public ConstraintNetwork translate(ConstraintNetwork cn) {
        ConstraintNetwork cp = cn.clone();
        Set<ConstraintNetwork> ret = new HashSet();
        cn.vertexSet().forEach( v -> {
                    if(dtrans.match(cp,v)) dtrans.translate(cp, v);
                    if(impl.match(cp,v)) impl.translate(cp, v);
                }
        );

        sanitize(cn);
        return cp;
    }

    public void sanitize(ConstraintNetwork cn) {
        Set<Node> toRm = new HashSet();
        for (Node v : cn.vertexSet()) {
            if (v.isOperand() && (cn.outDegreeOf(v) + cn.inDegreeOf(v) == 0)) {
                toRm.add(v);
            }
        }
        cn.removeAllVertices(toRm);
    }

}

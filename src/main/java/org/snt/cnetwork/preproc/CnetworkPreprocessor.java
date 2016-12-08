package org.snt.cnetwork.preproc;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Edge;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;

import java.util.*;

public enum CnetworkPreprocessor {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkPreprocessor.class);

    final static Map<NodeKind, Converter> conv = new HashMap();

    static {
        conv.put(NodeKind.OR, new DisjunctionTranslator());
        conv.put(NodeKind.IMPLIES, new ImplicationTranslator());
        conv.put(NodeKind.ITE, new IteTranslator());
    }



    private Node getNext(ConstraintNetwork cn) {
        Node ret = null;
        try {
            ret = cn.vertexSet()
                    .stream()
                    .filter(v -> conv.containsKey(v.getKind())).findFirst().get();
        } catch (NoSuchElementException e) {
            ;
        } finally {
            return ret;
        }
    }

    private void invokeFor(ConstraintNetwork cn, Node n) {
        assert conv.containsKey(n.getKind());
        LOGGER.debug("invoke for {}", n.getKind());
        conv.get(n.getKind()).translate(cn, n);
        LOGGER.debug(cn.toDot());
    }

    public ConstraintNetwork translate(ConstraintNetwork cn) {
        ConstraintNetwork cp = cn.clone();
        Node nxt;
        while((nxt = getNext(cp)) != null){
            invokeFor(cp,nxt);
        }
        sanitize(cp);
        return cp;
    }

    public void sanitize(ConstraintNetwork cn) {
        Set<Node> toRm = new HashSet();
        for (Node v : cn.vertexSet()) {
            if (v.isOperand() && (cn.outDegreeOf(v) + cn.inDegreeOf(v) == 0)) {
                toRm.add(v);
            }


            // remove double negation
            if(v.getKind() == NodeKind.NOT &&
                    cn.outDegreeOf(v) == 1 &&
                    cn.getConnectedOutNodes(v).iterator().next().getKind() ==
                    NodeKind.NOT) {

                Node out = cn.getConnectedOutNodes(v).iterator().next();

                Set<Node> vin = cn.getConnectedInNodes(v);

                assert vin.size() == 1;

                for(Edge e: cn.outgoingEdgesOf(out)){
                    Edge n = new Edge(vin.iterator().next(), e.getDestNode(), e
                            .getSequence
                            ());
                    cn.addConnection(n);
                }


                toRm.add(v);
                toRm.add(out);

            }
        }
        cn.removeAllVertices(toRm);
    }

}

package org.snt.cnetwork.core;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.exception.EUFInconsistencyException;


public class TestConstraintNetworkGeneration {

    final static Logger LOGGER = LoggerFactory.getLogger(TestConstraintNetworkGeneration.class);


    @Test
    public void testCnConstruction() {

        ConstraintNetworkBuilder tm2 = new ConstraintNetworkBuilder();
        Node x = new Operand("x", NodeKind.STRVAR);
        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, NodeKind.STRREXP);
        Node v1 = new Operand("sv7", NodeKind.NUMVAR);
        try {
            Node toStrV1 = tm2.addOperation(NodeKind.TOSTR, v1);
            Node orv1 = tm2.addOperation(NodeKind.CONCAT, or, toStrV1);
            Node eq = new Operand(" +\\>= +", NodeKind.STRREXP);
            Node orv1comp = tm2.addOperation(NodeKind.CONCAT, orv1, eq);
            Node v2 = new Operand("sv8", NodeKind.NUMVAR);
            Node toStrV2 = tm2.addOperation(NodeKind.TOSTR, v2);
            Node orv1compv2 = tm2.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);
            String scomment = "(\\<!\\-\\-|#)";
            Node comment = new Operand(scomment, NodeKind.STRREXP);

            tm2.addOperation(NodeKind.CONCAT, orv1compv2, comment);
            tm2.addConstraint(NodeKind.GREATEREQ, v1, v2);
            tm2.setStartNode(orv1compv2);
            tm2.addConstraint(NodeKind.MATCHES, x, orv1compv2);
        } catch (EUFInconsistencyException e) {
            Assert.assertTrue(false);
        }
        LOGGER.debug(tm2.getEufLattice().toDot());
        LOGGER.info(tm2.getConstraintNetwork().toDot());
    }


    @Test
    public void testCNClone() {
        ConstraintNetworkBuilder tm2 = new ConstraintNetworkBuilder();
        Node x = new Operand("x", NodeKind.STRVAR);
        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, NodeKind.STRREXP);
        Node v1 = new Operand("sv7", NodeKind.NUMVAR);

        try {
            Node toStrV1 = tm2.addOperation(NodeKind.TOSTR, v1);
            Node orv1 = tm2.addOperation(NodeKind.CONCAT, or, toStrV1);
            Node eq = new Operand(" +\\>= +", NodeKind.STRREXP);
            Node orv1comp = tm2.addOperation(NodeKind.CONCAT, orv1, eq);
            Node v2 = new Operand("sv8", NodeKind.NUMVAR);
            Node toStrV2 = tm2.addOperation(NodeKind.TOSTR, v2);
            Node orv1compv2 = tm2.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);
            String scomment = "(\\<!\\-\\-|#)";
            Node comment = new Operand(scomment, NodeKind.STRREXP);
            tm2.addOperation(NodeKind.CONCAT, orv1compv2, comment);
            tm2.addConstraint(NodeKind.GREATEREQ, v1, v2);
            tm2.setStartNode(orv1compv2);
            tm2.addConstraint(NodeKind.MATCHES, x, orv1compv2);
        } catch (EUFInconsistencyException e) {
            Assert.assertFalse(true);
        }

        LOGGER.info(tm2.getConstraintNetwork().toDot());
        ConstraintNetworkBuilder tm3 = tm2.clone();
        LOGGER.info(tm3.getConstraintNetwork().toDot());


        for (Edge e1 : tm2.edgeSet()) {
            for (Edge e2 : tm3.edgeSet()) {
                Assert.assertTrue(e1 != e2);
            }
        }

        for (Node n1 : tm2.vertexSet()) {
            for (Node n2 : tm3.vertexSet()) {
                Assert.assertTrue(n1 != n2);
            }
        }


    }


}



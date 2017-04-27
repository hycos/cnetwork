package org.snt.cnetwork.core;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.graph.*;
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
        LOGGER.debug(tm2.getExecutionTree().toDot());
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


    @Test
    public void testCopyConstructor() {

        ConstraintNetworkBuilder cb1 = new ConstraintNetworkBuilder();

        Node a = cb1.addOperand(NodeKind.STRVAR, "a");
        Node b = cb1.addOperand(NodeKind.STRVAR, "b");
        Node c = cb1.addOperand(NodeKind.STRVAR, "c");

        Node concat1 = null, concat2 = null;

        try {
            concat1 = cb1.addOperation(NodeKind.CONCAT, a, b);
            concat2 = cb1.addOperation(NodeKind.CONCAT, concat1, c);
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        ConstraintNetworkBuilder cb2 = new ConstraintNetworkBuilder(cb1);

        try {
            cb2.addConstraint(NodeKind.EQUALS, concat1, concat2);
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
        }


        Assert.assertEquals(cb1.vertexSet().size(), 5);
        Assert.assertEquals(cb2.vertexSet().size(), 6);

        Node d = cb1.addOperand(NodeKind.STRVAR, "d");

        try {
            cb1.addConstraint(NodeKind.EQUALS, d, d);
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
        }


        Assert.assertEquals(cb1.vertexSet().size(), 7);
        Assert.assertEquals(cb2.vertexSet().size(), 6);


        LOGGER.debug(cb1.getConstraintNetwork().toDot());

    }


    @Test
    public void testDummy() {

        ConstraintNetworkBuilder cb1 = new ConstraintNetworkBuilder();

        Node a = cb1.addOperand(NodeKind.STRVAR, "a");
        Node b = cb1.addOperand(NodeKind.STRVAR, "b");
        Node c = cb1.addOperand(NodeKind.STRVAR, "c");

        Node concat1 = null, concat2 = null;

        try {
            concat1 = cb1.addOperation(NodeKind.CONCAT, a, b);
            concat2 = cb1.addOperation(NodeKind.CONCAT, concat1, c);
            cb1.addConstraint(NodeKind.EQUALS, concat1, concat2);
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }


        LOGGER.debug("=========");

        LOGGER.debug("d {}", cb1.getEufLattice().debug());


        LOGGER.debug("=========");

    }


}



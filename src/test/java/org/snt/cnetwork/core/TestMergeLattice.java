package org.snt.cnetwork.core;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.euf.EquiClass;
import org.snt.cnetwork.core.euf.EufLattice;
import org.snt.cnetwork.core.euf.NodeElemFact;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;


public class TestMergeLattice {
    final static Logger LOGGER = LoggerFactory.getLogger(TestMergeLattice.class);

    @Test
    public void testSimple0() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufLattice<Node> mt = new EufLattice<>(new NodeElemFact(cn));

        Node a = cn.addOperand(NodeKind.STRLIT, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        try {
            mt.addEquiClass(a,b);
            mt.addEquiClass(a,b);
            mt.addEquiClass(a,b);
            mt.addEquiClass(a,b);
            mt.addEquiClass(a);
            mt.addEquiClass(b);


        } catch (EUFInconsistencyException e1) {
            Assert.assertTrue(false);
        }

        LOGGER.debug(mt.toDot());
        Assert.assertEquals(mt.vertexSet().size(),3);
    }

    @Test
    public void testSimple1() {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufLattice<Node> mt = new EufLattice<>(new NodeElemFact(cn));

        Node a = cn.addOperand(NodeKind.STRLIT, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node d = cn.addOperand(NodeKind.STRVAR, "d");
        Node e = cn.addOperand(NodeKind.STRVAR, "e");

        try {
            mt.addEquiClass(a,b);
            mt.addEquiClass(d);
            mt.addEquiClass(e);
            mt.addEquiClass(a,e);
            mt.addEquiClass(e,b);
            mt.addEquiClass(d,b);
        } catch (EUFInconsistencyException e1) {
            Assert.assertTrue(false);
        }

        LOGGER.debug(mt.toDot());
        Assert.assertEquals(mt.vertexSet().size(),3);
    }



    @Test
    public void testSimple2() {

        LOGGER.debug("SIMPLE 2");
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufLattice<Node> mt = new EufLattice<>(new NodeElemFact(cn));

        Node a = cn.addOperand(NodeKind.STRLIT, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node d = cn.addOperand(NodeKind.STRVAR, "d");
        Node e = cn.addOperand(NodeKind.STRVAR, "e");

        EquiClass ab = null;
        try {
            ab = mt.addEquiClass(a,b);
            mt.addEquiClass(d);
            mt.addEquiClass(e);
        } catch (EUFInconsistencyException e1) {
            Assert.assertTrue(false);
        }
        LOGGER.debug(mt.toDot());

        EquiClass join = null;

        try {
            join = mt.join(a,b);
            LOGGER.debug("JOIN {}", join);
        } catch (MissingItemException e1) {
            Assert.assertFalse(true);
        }

        LOGGER.debug("JOIN 1 {}", join);
        LOGGER.debug("ab {}", ab);
        Assert.assertEquals(join, ab);

        try {
            join = mt.join(a,d);
        } catch (MissingItemException e1) {
            Assert.assertFalse(true);
        }

        Assert.assertEquals(new EquiClass.Top(), join);
        LOGGER.debug(mt.toDot());
    }

    @Test
    public void testOperation() {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);
        EufLattice<Node> mt = cn.getEufLattice();


        Node a = cn.addOperand(NodeKind.STRLIT, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node k = cn.addOperand(NodeKind.STRVAR, "k");

        try {
            Node concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
            Node concat4 = cn.addOperation(NodeKind.CONCAT, k, k);
            mt.addEquiClass(concat1,concat4);
            mt.addEquiClass(a,k);
        } catch (EUFInconsistencyException e1) {
            Assert.assertFalse(true);
        }

        LOGGER.debug(mt.toDot());

        Assert.assertEquals(mt.vertexSet().size(), 4);
    }

    @Test
    public void testRecursion() {


        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);
        EufLattice<Node> mt = cn.getEufLattice();

        Node a = cn.addOperand(NodeKind.STRLIT, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node k = cn.addOperand(NodeKind.STRVAR, "k");

        try {
            Node concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
            Node concat2 = cn.addOperation(NodeKind.CONCAT, a, k);

            mt.addEquiClass(concat1, k);
            mt.addEquiClass(concat2, k);
        } catch (EUFInconsistencyException e) {
            Assert.assertFalse(true);
        }

        LOGGER.debug(mt.toDot());
    }

    @Test
    public void testConsistency() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);
        EufLattice<Node> mt = cn.getEufLattice();

        Node a = cn.addOperand(NodeKind.STRLIT, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node d = cn.addOperand(NodeKind.STRVAR, "d");
        Node e = cn.addOperand(NodeKind.STRVAR, "e");
        Node f = cn.addOperand(NodeKind.STRVAR, "f");
        Node g = cn.addOperand(NodeKind.STRVAR, "g");

        boolean thrown = false;

        try {
            mt.addEquiClass(a,b);
            mt.addEquiClass(d);
            mt.addEquiClass(e);
            mt.addEquiClass(f,g);
            mt.addInequialityConstraint(b,f);
        } catch (EUFInconsistencyException e1) {
            thrown = true;
            LOGGER.debug(e1.getMessage());
        }

        LOGGER.debug(mt.toDot());

        Assert.assertEquals(thrown, false);

        try {
            mt.addEquiClass(b,g);
        } catch (EUFInconsistencyException e2) {
            LOGGER.debug(e2.getMessage());
            thrown = true;
        }

        Assert.assertTrue(thrown);


        LOGGER.debug(mt.toDot());

    }

    @Test
    public void testConsistency2() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);
        EufLattice<Node> mt = new EufLattice<>(new NodeElemFact(cn));

        Node a = cn.addOperand(NodeKind.STRLIT, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node d = cn.addOperand(NodeKind.STRVAR, "d");
        Node e = cn.addOperand(NodeKind.STRVAR, "e");
        Node f = cn.addOperand(NodeKind.STRVAR, "f");
        Node g = cn.addOperand(NodeKind.STRVAR, "g");

        boolean thrown = false;

        try {
            mt.addEquiClass(a,b);
            mt.addEquiClass(d);
            mt.addEquiClass(e);
            mt.addEquiClass(f,g);
            mt.addEquiClass(b,g);
        } catch (EUFInconsistencyException e1) {
            thrown = true;
        }

        Assert.assertFalse(thrown);

        LOGGER.debug(mt.toDot());

        try {
            mt.addInequialityConstraint(b,f);
        } catch (EUFInconsistencyException e2) {
            LOGGER.debug("+++ " + e2.getMessage());
            thrown = true;
        }

        Assert.assertTrue(thrown);

        LOGGER.debug(mt.toDot());
    }


    @Test
    public void testInferenceCase1() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node one = cn.addOperand(NodeKind.NUMLIT, "1");
        Node i = cn.addOperand(NodeKind.NUMVAR, "i");
        Node five = cn.addOperand(NodeKind.NUMVAR, "5");
        Node k = cn.addOperand(NodeKind.NUMVAR, "k");

        try {
            Node idxof = cn.addOperation(NodeKind.INDEXOF, a, one);
            cn.addConstraint(NodeKind.EQUALS, idxof, i);
            cn.addConstraint(NodeKind.EQUALS, a, b);
            LOGGER.debug("=====================================================");
            LOGGER.debug(cn.getConstraintNetwork().toDot());
            Node aliasidxof = cn.addOperation(NodeKind.INDEXOF, b, one);
            //cn.addConstraint(NodeKind.EQUALS, aliasidxof, k);

            Assert.assertEquals(aliasidxof, idxof);

        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        LOGGER.debug(cn.getEufLattice().toDot());
        LOGGER.debug(cn.getConstraintNetwork().toDot());
    }

    @Test
    public void testInferenceCase2() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");

        Node one = cn.addOperand(NodeKind.NUMLIT, "1");
        Node i = cn.addOperand(NodeKind.NUMVAR, "i");
        Node k = cn.addOperand(NodeKind.NUMVAR, "k");

        try {
            Node idxof = cn.addOperation(NodeKind.INDEXOF, a, one);
            cn.addConstraint(NodeKind.EQUALS, idxof, i);
            Node aliasidxof = cn.addOperation(NodeKind.INDEXOF, b, one);
            cn.addConstraint(NodeKind.EQUALS, aliasidxof, k);
            cn.addConstraint(NodeKind.EQUALS, a, b);
            //cn.addConstraint(NodeKind.EQUALS, aliasidxof, one);


            Node na = cn.getNodeByLabel(idxof.getLabel());
            Node nb = cn.getNodeByLabel(aliasidxof.getLabel());

            Assert.assertEquals(na, nb);
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        LOGGER.debug(cn.getEufLattice().toDot());
        LOGGER.debug(cn.getConstraintNetwork().toDot());
    }


    @Test
    public void testInferenceCase3() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node c = cn.addOperand(NodeKind.STRVAR, "c");
        Node d = cn.addOperand(NodeKind.STRVAR, "d");


        try {
            Node concat = cn.addOperation(NodeKind.CONCAT,a,b);
            Node concat2 = cn.addOperation(NodeKind.CONCAT,c,concat);
            Node eq = cn.addConstraint(NodeKind.EQUALS,concat,d);
            Node concat3 = cn.addOperation(NodeKind.CONCAT,c,d);

            Assert.assertEquals(concat2, concat3);
            LOGGER.debug("concat 3 {}", concat3.getLabel());

        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        LOGGER.debug(cn.getEufLattice().toDot());

    }


    @Test
    public void testInferenceCase4() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");



        try {
            Node concat1 = cn.addOperation(NodeKind.CONCAT,a,b);
            Node concat2 = cn.addOperation(NodeKind.CONCAT, concat1, b);

            Node eq0 = cn.addConstraint(NodeKind.EQUALS, concat1, a);
            Node eq1 = cn.addConstraint(NodeKind.EQUALS, concat2, a);

            Node na = cn.getNodeByLabel(concat1.getLabel());
            Node nb = cn.getNodeByLabel(concat2.getLabel());

            Assert.assertEquals(na, nb);

        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        LOGGER.debug(cn.getEufLattice().toDot());

    }




}

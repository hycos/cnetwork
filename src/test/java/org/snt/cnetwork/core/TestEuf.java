package org.snt.cnetwork.core;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.euf.EquiClass;
import org.snt.cnetwork.core.euf.EufManager;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;


public class TestEuf {
    final static Logger LOGGER = LoggerFactory.getLogger(TestEuf.class);

    @Test
    public void testSimple0() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufManager mt = new EufManager(cn);

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

        LOGGER.debug(mt.getLattice().toDot());
        Assert.assertEquals(mt.getLattice().vertexSet().size(),3);
    }

    @Test
    public void testSimple1() {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufManager mt = new EufManager(cn);

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
            LOGGER.debug("ERROR {}", e1.getMessage());
            Assert.assertTrue(false);
        }

        LOGGER.debug(mt.getLattice().toDot());
        Assert.assertEquals(mt.getLattice().vertexSet().size(),3);
    }



    @Test
    public void testSimple2() {

        LOGGER.debug("SIMPLE 2");

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufManager mt = new EufManager(cn);

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
        LOGGER.debug(mt.getLattice().toDot());

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
        LOGGER.debug(mt.getLattice().toDot());
    }

    @Test
    public void testOperation() {


        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufManager mt = new EufManager(cn);


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

        LOGGER.debug(mt.getLattice().toDot());

        Assert.assertEquals(mt.getLattice().vertexSet().size(), 7);
    }

    @Test
    public void testRecursion() {


        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufManager mt = new EufManager(cn);

        Node a = cn.addOperand(NodeKind.STRLIT, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node k = cn.addOperand(NodeKind.STRVAR, "k");

        try {
            //Node concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
            Node concat1 = cn.addOperation(NodeKind.CONCAT, a, k);

            mt.addEquiClass(concat1, k);
            //mt.addEquiClass(concat2, k);
        } catch (EUFInconsistencyException e) {
            Assert.assertFalse(true);
        }

        LOGGER.debug(mt.getLattice().toDot());
        LOGGER.debug(cn.getConstraintNetwork().toDot());
    }

    @Test
    public void testConsistency() {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufManager mt = new EufManager(cn);

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

        LOGGER.debug(mt.getLattice().toDot());

        Assert.assertEquals(thrown, false);

        try {
            mt.addEquiClass(b,g);
        } catch (EUFInconsistencyException e2) {
            LOGGER.debug(e2.getMessage());
            thrown = true;
        }

        Assert.assertTrue(thrown);


        //LOGGER.debug(mt.toDot());

    }

    @Test
    public void testConsistency2() {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufManager mt = new EufManager(cn);

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

        LOGGER.debug(mt.getLattice().toDot());

        try {
            mt.addInequialityConstraint(b,f);
        } catch (EUFInconsistencyException e2) {
            LOGGER.debug("+++ " + e2.getMessage());
            thrown = true;
        }

        Assert.assertTrue(thrown);

        LOGGER.debug(mt.getLattice().toDot());
    }



    @Test
    public void testInferenceCase1() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node one = cn.addOperand(NodeKind.STRVAR, "1");
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
            cn.addConstraint(NodeKind.EQUALS, aliasidxof, k);

            LOGGER.debug(cn.getConstraintNetwork().toDot());
            LOGGER.debug(cn.getEufLattice().toDot());
            LOGGER.debug("alias {}", aliasidxof.getId());
            LOGGER.debug("orig {}", idxof.getId());
            Assert.assertEquals(cn.inferEquivalentNode(aliasidxof),cn
                    .inferEquivalentNode(idxof));

        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        LOGGER.debug(cn.getEufLattice().toDot());
        LOGGER.debug(cn.getConstraintNetwork().toDot());
    }

    @Test
    public void testInferenceBackwards() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");

        Node five = cn.addOperand(NodeKind.NUMLIT, "5");
        Node one = cn.addOperand(NodeKind.STRLIT, "1");
        Node i = cn.addOperand(NodeKind.NUMVAR, "i");
        Node k = cn.addOperand(NodeKind.NUMVAR, "k");

        try {
            Node idxof = cn.addOperation(NodeKind.INDEXOF, a, one);
            cn.addConstraint(NodeKind.EQUALS, idxof, i);
            Node aliasidxof = cn.addOperation(NodeKind.INDEXOF, b, one);
            cn.addConstraint(NodeKind.EQUALS, aliasidxof, k);
            cn.addConstraint(NodeKind.EQUALS, a, b);
            cn.addConstraint(NodeKind.GREATER, k, five);

            LOGGER.debug(cn.getEufLattice().toDot());
            LOGGER.debug(cn.getConstraintNetwork().toDot());
            Node na = cn.inferEquivalentNode(idxof);
            Node nb = cn.inferEquivalentNode(aliasidxof);

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
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node c = cn.addOperand(NodeKind.STRVAR, "c");
        Node d = cn.addOperand(NodeKind.STRVAR, "d");

        // concat (a,b)
        // concat (c, concat(a,b)
        // concat (a,b),d
        // concat(c, concat(a,b)) = concat(c,d)
        try {
            Node concat = cn.addOperation(NodeKind.CONCAT,a,b);
            Node concat2 = cn.addOperation(NodeKind.CONCAT,c,concat);
            Node eq = cn.addConstraint(NodeKind.EQUALS,concat,d);
            Node concat3 = cn.addOperation(NodeKind.CONCAT,c,d);
            Assert.assertEquals(concat2, concat3);
            //LOGGER.debug("concat 3 {}", concat3.getLabel());

        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        LOGGER.debug(cn.getEufLattice().toDot());

    }


    @Test
    public void testInferenceCase4() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");



        try {
            Node concat1 = cn.addOperation(NodeKind.CONCAT,a,b);
            Node concat2 = cn.addOperation(NodeKind.CONCAT, concat1, b);

            Node eq0 = cn.addConstraint(NodeKind.EQUALS, concat1, a);
            Node eq1 = cn.addConstraint(NodeKind.EQUALS, concat2, a);

            Node na = cn.inferEquivalentNode(concat1);
            Node nb = cn.inferEquivalentNode(concat2);

            Assert.assertEquals(na, nb);

        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        LOGGER.debug(cn.getEufLattice().toDot());

    }



    @Test
    public void testInferenceCase5() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");
        Node five = cn.addOperand(NodeKind.STRLIT, "5");

        try {
            cn.addConstraint(NodeKind.EQUALS, a, b);
            cn.addConstraint(NodeKind.EQUALS, a, five);
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
        }

        Assert.assertEquals(cn.vertexSet().size(), 3);

        LOGGER.debug(cn.getConstraintNetwork().toDot());

    }

    @Test
    public void testDoubleAdd() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node a = cn.addOperand(NodeKind.STRVAR, "a");
        Node b = cn.addOperand(NodeKind.STRVAR, "b");

        try {
            Node concatab1 = cn.addOperation(NodeKind.CONCAT, a,b);
            Node concatab2 = cn.addOperation(NodeKind.CONCAT, a,b);
            Assert.assertEquals(concatab1, concatab2);
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
        }

        LOGGER.debug(cn.getEufLattice().toDot());
        LOGGER.debug(cn.getConstraintNetwork().toDot());
    }

    @Test
    public void testSameConcat() {

        ConstraintNetworkBuilder cb1 = new ConstraintNetworkBuilder();

        Node a = cb1.addOperand(NodeKind.STRVAR, "a");
        Node b = cb1.addOperand(NodeKind.STRVAR, "b");
        Node c = cb1.addOperand(NodeKind.STRVAR, "c");

        Node concat1 = null, concat2 = null, eq;

        try {
            concat1 = cb1.addOperation(NodeKind.CONCAT, a, b);
            concat2 = cb1.addOperation(NodeKind.CONCAT, concat1, c);
            eq = cb1.addConstraint(NodeKind.EQUALS, concat1, concat2);

            //concat3 = cb1.addOperation(NodeKind.CONCAT, concat2,)

        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
            Assert.assertFalse(true);
        }

        LOGGER.debug(cb1.getConstraintNetwork().toDot());

        Assert.assertEquals(cb1.vertexSet().size(), 6);
    }

    @Test
    public void testLitAdd() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node a = cn.addOperand(NodeKind.STRLIT, ".");
        Node b = cn.addOperand(NodeKind.STRLIT, ".");
        LOGGER.debug(cn.getEufLattice().toDot());
        LOGGER.debug(cn.getConstraintNetwork().toDot());
    }





}

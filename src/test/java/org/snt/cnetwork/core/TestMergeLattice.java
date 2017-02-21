package org.snt.cnetwork.core;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.mergelattice.EquiClass;
import org.snt.cnetwork.core.mergelattice.EufLattice;
import org.snt.cnetwork.core.mergelattice.NodeElemFact;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;


public class TestMergeLattice {
    final static Logger LOGGER = LoggerFactory.getLogger(TestMergeLattice.class);

    @Test
    public void testSimple0() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        EufLattice<Node> mt = new EufLattice<>(new NodeElemFact(cn));

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
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

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");

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

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");

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


        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");
        Operand k = cn.addOperand(NodeKind.STRVAR, "k");

        try {
            Operation concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
            Operation concat4 = cn.addOperation(NodeKind.CONCAT, k, k);
            mt.addEquiClass(concat1,concat4);
            mt.addEquiClass(e);
        } catch (EUFInconsistencyException e1) {
            Assert.assertFalse(true);
        }

        LOGGER.debug(mt.toDot());

        Assert.assertEquals(mt.vertexSet().size(), 7);
    }

    @Test
    public void testRecursion() {


        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);
        EufLattice<Node> mt = cn.getEufLattice();

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand k = cn.addOperand(NodeKind.STRVAR, "k");

        try {
            Operation concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
            Operation concat2 = cn.addOperation(NodeKind.CONCAT, a, k);

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

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");
        Operand f = cn.addOperand(NodeKind.STRVAR, "f");
        Operand g = cn.addOperand(NodeKind.STRVAR, "g");

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
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(false);
        EufLattice<Node> mt = new EufLattice<>(new NodeElemFact(cn));

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");
        Operand f = cn.addOperand(NodeKind.STRVAR, "f");
        Operand g = cn.addOperand(NodeKind.STRVAR, "g");

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

        /**try {
            mt.addInequialityConstraint(b,f);
        } catch (EUFInconsistencyException e2) {
            LOGGER.debug("+++ " + e2.getMessage());
            thrown = true;
        }

        Assert.assertTrue(thrown);


        LOGGER.debug(mt.toDot());**/

    }


    @Test
    public void testInference() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(true);
        EufLattice<Node> mt = cn.getEufLattice();
        NodeElemFact nf = cn.getNodeElementFact();


        Operand a = cn.addOperand(NodeKind.STRVAR, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand one = cn.addOperand(NodeKind.NUMLIT, "1");
        Operand i = cn.addOperand(NodeKind.NUMVAR, "i");
        Operand five = cn.addOperand(NodeKind.NUMVAR, "5");

        try {
            Operation idxof = cn.addOperation(NodeKind.INDEXOF, a, one);
            cn.addConstraint(NodeKind.EQUALS, idxof, i);
            cn.addConstraint(NodeKind.EQUALS, a, b);

            Operation aliasidxof = cn.addOperation(NodeKind.INDEXOF, b, one);


            cn.addConstraint(NodeKind.EQUALS, aliasidxof, five);

            //Collection<EquiClass> n = nf.createEquiClasses(aliasidxof);

            //EquiClass foo = n.stream().filter(v -> v.isNested()).findFirst()
            //        .get();

            //EquiClass sup = mt.inferEquiClassFor(foo);

            //LOGGER.debug("eq {}", sup.getDotLabel());

            //EquiClass ife = sup.union(foo);
            //LOGGER.debug("ife {}", ife.getDotLabel());
            //mt.addEquiClass(ife);

        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
        }


        LOGGER.debug(mt.toDot());

    }



}

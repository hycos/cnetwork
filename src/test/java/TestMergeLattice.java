import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.core.mergelattice.EquiClass;
import org.snt.cnetwork.core.mergelattice.MergeLattice;
import org.snt.cnetwork.core.mergelattice.NodeElemFact;
import org.snt.cnetwork.exception.MissingItemException;


public class TestMergeLattice {
    final static Logger LOGGER = LoggerFactory.getLogger(TestMergeLattice.class);

    @Test
    public void testSimple1() {

        ConstraintNetwork cn = new ConstraintNetwork();
        MergeLattice<Node> mt = new MergeLattice<>(new NodeElemFact(cn));

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");

        mt.addEquiClass(a,b);
        mt.addEquiClass(d);
        mt.addEquiClass(e);
        mt.addEquiClass(a,e);
        mt.addEquiClass(e,b);
        mt.addEquiClass(d,b);

        assert mt.vertexSet().size() == 7;
    }

    @Test
    public void testSimple2() {

        LOGGER.debug("SIMPLE 2");
        ConstraintNetwork cn = new ConstraintNetwork();
        MergeLattice<Node> mt = new MergeLattice<>(new NodeElemFact(cn));

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");

        EquiClass ab = mt.addEquiClass(a,b);
        mt.addEquiClass(d);
        mt.addEquiClass(e);
        LOGGER.debug(mt.toDot());

        EquiClass join = null;

        try {
            join = mt.join(a,b);
        } catch (MissingItemException e1) {
            e1.printStackTrace();
        }

        Assert.assertEquals(join, ab);


        LOGGER.debug(mt.toDot());

    }

    @Test
    public void testOperation() {

        ConstraintNetwork cn = new ConstraintNetwork();
        MergeLattice<Node> mt = new MergeLattice<>(new NodeElemFact(cn));


        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");
        Operand k = cn.addOperand(NodeKind.STRVAR, "k");

        Operation concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
        Operation concat4 = cn.addOperation(NodeKind.CONCAT, k, k);

        mt.addEquiClass(concat1,concat4);
        LOGGER.debug(mt.toDot());

        assert mt.vertexSet().size() == 16;
    }

    @Test
    public void testRecursion() {

        ConstraintNetwork cn = new ConstraintNetwork();
        MergeLattice<Node> mt = new MergeLattice<>(new NodeElemFact(cn));


        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand k = cn.addOperand(NodeKind.STRVAR, "k");

        Operation concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
        Operation concat2 = cn.addOperation(NodeKind.CONCAT, a, k);

        mt.addEquiClass(concat1,k);
        mt.addEquiClass(concat2,k);


        LOGGER.debug(mt.toDot());
    }
}

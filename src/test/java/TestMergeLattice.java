import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.core.Operand;
import org.snt.cnetwork.core.mergelattice.MergeLattice;
import org.snt.cnetwork.core.mergelattice.NodeElemFact;


public class TestMergeLattice {
    final static Logger LOGGER = LoggerFactory.getLogger(TestMergeLattice.class);

    @Test
    public void testSimple() {
        MergeLattice<Node> mt = new MergeLattice<>(new NodeElemFact());

        ConstraintNetwork cn = new ConstraintNetwork();

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand c = cn.addOperand(NodeKind.STRVAR, "c");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");

        //Operation concat = cn.addOperation(NodeKind.CONCAT, a, b);


        mt.addEquiClasses(new Node[]{a});
        //mt.addEquiClass(new ElementTuple(b), new ElementTuple(c));
        //mt.addEquiClass(new ElementTuple(d));
        //mt.addEquiClass(new ElementTuple(e));
        //mt.addEquiClass(new ElementTuple(a), new ElementTuple(e));
        //mt.addEquiClass(new ElementTuple(e), new ElementTuple(b));
        //mt.addEquiClass(new ElementTuple(d), new ElementTuple(c));


        LOGGER.debug(mt.toDot());
        //assert mt.vertexSet().size() == 8;
    }

    /**@Test
    public void testOperation() {
        MergeLattice mt = new MergeLattice(new NodeTuple.NodeBagFact());

        ConstraintNetwork cn = new ConstraintNetwork();

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand c = cn.addOperand(NodeKind.STRVAR, "c");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");
        Operand g = cn.addOperand(NodeKind.STRVAR, "g");

        Operation concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
        Operation concat2 = cn.addOperation(NodeKind.CONCAT, c, b);


        mt.addEquiClass(new NodeTuple(concat1, cn.getParametersFor(concat1)));
        mt.addEquiClass(new NodeTuple(concat2, cn.getParametersFor(concat2)));
        mt.addEquiClass(new NodeTuple(b), new NodeTuple(c));
        //mt.addEquiClass(new NodeBag(a), new NodeBag(e));
        //mt.addEquiClass(new NodeBag(e), new NodeBag(g));
        //mt.addEquiClass(new NodeBag(d));
        //mt.addEquiClass(new NodeBag(e));
        //mt.addEquiClass(new NodeBag(a), new NodeBag(e));
        //mt.addEquiClass(new NodeBag(e), new NodeBag(b));
        //mt.addEquiClass(new NodeBag(d), new NodeBag(c));

        //assert mt.vertexSet().size() == 8;

        LOGGER.debug(mt.toDot());
    }**/
}

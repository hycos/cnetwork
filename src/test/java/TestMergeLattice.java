import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.core.mergelattice.MergeLattice;
import org.snt.cnetwork.core.mergelattice.NodeElemFact;


public class TestMergeLattice {
    final static Logger LOGGER = LoggerFactory.getLogger(TestMergeLattice.class);

    @Test
    /**public void testSimple() {
        MergeLattice<Node> mt = new MergeLattice<>(new NodeElemFact());

        ConstraintNetwork cn = new ConstraintNetwork();

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand c = cn.addOperand(NodeKind.STRVAR, "c");
        Operand d = cn.addOperand(NodeKind.STRVAR, "d");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");

        //Operation concat = cn.addOperation(NodeKind.CONCAT, a, b);


        mt.addEquiClasses(new Node[]{a,b},new Node[]{d},new Node[]{e},
                new Node[]{a,e},new Node[]{e,b},new Node[]{d,b});

        LOGGER.debug(mt.toDot());
        assert mt.vertexSet().size() == 7;
    }**/

    //@Test
    public void testOperation() {
        MergeLattice<Node> mt = new MergeLattice<>(new NodeElemFact());

        ConstraintNetwork cn = new ConstraintNetwork();

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand e = cn.addOperand(NodeKind.STRVAR, "e");

        Operation concat1 = cn.addOperation(NodeKind.CONCAT, a, b);
        Operation concat2 = cn.addOperation(NodeKind.CONCAT, b, e);


        mt.addEquiClass(new Node []{concat1,a,b});
        mt.addEquiClass(new Node []{a,e});

        //mt.addEquiClass(new NodeTuple(b), new NodeTuple(c));
        //mt.addEquiClass(new NodeBag(a), new NodeBag(e));
        //mt.addEquiClass(new NodeBag(e), new NodeBag(g));
        //mt.addEquiClass(new NodeBag(d));
        //mt.addEquiClass(new NodeBag(e));
        //mt.addEquiClass(new NodeBag(a), new NodeBag(e));
        //mt.addEquiClass(new NodeBag(e), new NodeBag(b));
        //mt.addEquiClass(new NodeBag(d), new NodeBag(c));

        //assert mt.vertexSet().size() == 8;

        LOGGER.debug(mt.toDot());
    }
}

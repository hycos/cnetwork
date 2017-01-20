import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.core.Operand;
import org.snt.cnetwork.core.Operation;
import org.snt.cnetwork.tools.mtree.MergeTree;
import org.snt.cnetwork.tools.mtree.NodeBag;


public class TestMergeTree {
    final static Logger LOGGER = LoggerFactory.getLogger(TestMergeTree.class);

    @Test
    public void testAddNodes() {
        MergeTree mt = new MergeTree();

        ConstraintNetwork cn = new ConstraintNetwork();

        Operand a = cn.addOperand(NodeKind.STRLIT, "a");
        Operand b = cn.addOperand(NodeKind.STRVAR, "b");
        Operand c = cn.addOperand(NodeKind.STRVAR, "c");

        Operation concat = cn.addOperation(NodeKind.CONCAT, a, b);


        mt.addVertex(new NodeBag(concat, cn.getParametersFor(concat)), new
                NodeBag(b));

        //mt.addVertex(new NodeBag(b),new NodeBag(c));

        LOGGER.debug(mt.toDot());



    }
}

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.slicer.CnetworkSlicerBackward;
import org.snt.cnetwork.utils.CnetworkTool;

import java.io.IOException;
import java.util.Collection;


public class TestCnetworkTool {

    final static Logger LOGGER = LoggerFactory.getLogger(TestCnetworkTool.class);

    private final static ConstraintNetwork cn = new ConstraintNetwork();


    @Before
    public void init() throws IOException {
        Operand op = new Operand("s0", NodeKind.STRVAR);
        Operand lit = new Operand("hello", NodeKind.STRLIT);
        Operand lit2 = new Operand("world", NodeKind.STRLIT);
        Operation op1 = cn.addOperation(NodeKind.CONTAINS,op,lit);
        Operation op2 = cn.addOperation(NodeKind.CONTAINS,op,lit2);
        cn.addConstraint(NodeKind.OR,op1,op2);
        LOGGER.debug(cn.toDot());
    }



    @Test
    public void testSlice() {

        Node crit = cn.getNodeById(3);
        CnetworkSlicerBackward bw = new CnetworkSlicerBackward(cn);

        Collection<Node> ret = bw.slice(crit);

        ConstraintNetwork sub = cn.subgraph(ret);
        LOGGER.debug("sub {}", sub.toDot());


        for(Edge e1 : cn.edgeSet()) {
            for (Edge e2 : sub.edgeSet()) {
                if(e1.getSrcNode().getId() == e2.getSrcNode().getId() &&
                        e1.getDestNode().getId() == e2.getDestNode().getId()) {
                    Assert.assertTrue(e1 == e2);
                }
            }
        }

        for(Node n1: cn.vertexSet()) {
            for (Node n2 : sub.vertexSet()) {
                if(n1.getId() == n2.getId()) {
                    Assert.assertTrue(n1 == n2);
                }
            }
        }

        ConstraintNetwork copy = sub.clone();

        for(Edge e1 : sub.edgeSet()) {
            for (Edge e2 : copy.edgeSet()) {
                if(e1.getSrcNode().getId() == e2.getSrcNode().getId() &&
                        e1.getDestNode().getId() == e2.getDestNode().getId()) {
                    Assert.assertFalse(e1 == e2);
                }
            }
        }

        for(Node n1: sub.vertexSet()) {
            for (Node n2 : copy.vertexSet()) {
                if(n1.getId() == n2.getId()) {
                    Assert.assertFalse(n1 == n2);
                }
            }
        }


        LOGGER.debug(copy.toDot());
    }

    @Test
    public void testCnSplit() {



        Collection<ConstraintNetwork> ret = CnetworkTool.INSTANCE
                .removeDisjunctions(cn);

        Assert.assertEquals(ret.size(),2);


        ret.forEach(c->
            LOGGER.debug(c.toDot())
        );


    }

}



import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.slicer.CnetworkSlicerBackward;
import org.snt.cnetwork.utils.CnetworkManipulator;

import java.io.IOException;
import java.util.Collection;


public class TestCnetworkTool {

    final static Logger LOGGER = LoggerFactory.getLogger(TestCnetworkTool.class);

    private final static ConstraintNetwork cn0 = new ConstraintNetwork();
    private final static ConstraintNetwork cn1 = new ConstraintNetwork();


    @Before
    public void init() throws IOException {
        Operand s0 = new Operand("s0", NodeKind.STRVAR);
        Operand hello = new Operand("hello", NodeKind.STRLIT);
        Operand world = new Operand("world", NodeKind.STRLIT);
        Operation containsHello = cn0.addOperation(NodeKind.CONTAINS,s0,hello);
        Operation containsWorld = cn0.addOperation(NodeKind.CONTAINS,s0,world);
        cn0.addConstraint(NodeKind.OR,containsHello,containsWorld);
        LOGGER.debug(cn0.toDot());


        Operand s1 = new Operand("s1", NodeKind.STRVAR);
        Operand s2 = new Operand("s2", NodeKind.STRVAR);
        Operand foo = new Operand("foo", NodeKind.STRLIT);
        Operand bar = new Operand("bar", NodeKind.STRLIT);
        Operation containsS1Foo = cn1.addOperation(NodeKind.CONTAINS,s1,foo);
        Operation containsS1Bar = cn1.addOperation(NodeKind.CONTAINS,s1,bar);

        Operation containsS2Foo = cn1.addOperation(NodeKind.CONTAINS,s2,foo);
        Operation containsS2Bar = cn1.addOperation(NodeKind.CONTAINS,s2,bar);

        cn1.addConstraint(NodeKind.OR,containsS1Foo,containsS1Bar);
        cn1.addConstraint(NodeKind.OR,containsS2Foo,containsS2Bar);
        LOGGER.debug("-------------");
        LOGGER.debug(cn1.toDot());
        LOGGER.debug("-------------");
    }



    @Test
    public void testSlice() {

        Node crit = cn0.getNodeById(3);
        CnetworkSlicerBackward bw = new CnetworkSlicerBackward(cn0);

        Collection<Node> ret = bw.slice(crit);

        ConstraintNetwork sub = cn0.subgraph(ret);
        LOGGER.debug("sub {}", sub.toDot());


        for(Edge e1 : cn0.edgeSet()) {
            for (Edge e2 : sub.edgeSet()) {
                if(e1.getSrcNode().getId() == e2.getSrcNode().getId() &&
                        e1.getDestNode().getId() == e2.getDestNode().getId()) {
                    Assert.assertTrue(e1 == e2);
                }
            }
        }

        for(Node n1: cn0.vertexSet()) {
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

        Assert.assertEquals(cn0.vertexSet().size(), 6);

        Collection<ConstraintNetwork> ret = CnetworkManipulator.INSTANCE
                .removeDisjunctions(cn0);

        Assert.assertEquals(ret.size(),2);


        ret.forEach(c-> {
            LOGGER.debug(c.toDot());
            Assert.assertEquals(c.vertexSet().size(), 3);
            }
        );
    }

    @Test
    public void testMultipleSplits() {


        Collection<ConstraintNetwork> ret = CnetworkManipulator.INSTANCE
                .removeDisjunctions(cn1);

        LOGGER.debug("CN {}", ret.size());
        ret.forEach(c-> {
                    LOGGER.debug(c.toDot());
                }
        );

    }

}



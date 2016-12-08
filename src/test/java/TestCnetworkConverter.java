import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.slicer.CnetworkSlicerBackward;
import org.snt.cnetwork.preproc.CnetworkPreprocessor;

import java.io.IOException;
import java.util.Collection;


public class TestCnetworkConverter {

    final static Logger LOGGER = LoggerFactory.getLogger(TestCnetworkConverter.class);

    private final static ConstraintNetwork cn0 = new ConstraintNetwork();
    private final static ConstraintNetwork cn1 = new ConstraintNetwork();
    private final static ConstraintNetwork cn2 = new ConstraintNetwork();
    private final static ConstraintNetwork cn3 = new ConstraintNetwork();


    @Before
    public void init() throws IOException {
        Operand s0 = new Operand("s0", NodeKind.STRVAR);
        Operand hello = new Operand("hello", NodeKind.STRLIT);
        Operand world = new Operand("world", NodeKind.STRLIT);
        Operation containsHello = cn0.addOperation(NodeKind.CONTAINS,s0,hello);
        Operation containsWorld = cn0.addOperation(NodeKind.CONTAINS,s0,world);
        cn0.addConstraint(NodeKind.OR,containsHello,containsWorld);


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



        Operand s3 = new Operand("s3", NodeKind.STRVAR);
        Operand s4 = new Operand("s4", NodeKind.STRVAR);
        Operand simple = new Operand("simple", NodeKind.STRLIT);
        Operand test = new Operand("test", NodeKind.STRLIT);

        Operation containsS3Simple = cn1.addOperation(NodeKind.CONTAINS,s3,
                simple);
        Operation containsS4Test = cn1.addOperation(NodeKind.CONTAINS,s4,test);

        cn2.addConstraint(NodeKind.IMPLIES, containsS3Simple, containsS4Test);



        Operand n0 = new Operand("n0", NodeKind.NUMVAR);
        Operand n1 = new Operand("n1", NodeKind.NUMVAR);
        Operand s5 = new Operand("s5", NodeKind.STRVAR);
        Operand s6 = new Operand("s6", NodeKind.STRVAR);
        Operand xy = new Operand("xy", NodeKind.STRLIT);
        Operand z = new Operand("z", NodeKind.STRLIT);

        Operation cond = cn3.addOperation(NodeKind.GREATER, n0, n1);

        Operation containsS6xy = cn3.addOperation(NodeKind.CONTAINS,s5,
                xy);
        Operation containsS6z = cn3.addOperation(NodeKind.CONTAINS,s6,z);

        cn3.addConstraint(NodeKind.ITE, cond, containsS6xy, containsS6z);

        LOGGER.debug(cn3.toDot());

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
    public void testDisjunctionTranslator() {


        ConstraintNetwork ret = CnetworkPreprocessor.INSTANCE.translate(cn0);


        LOGGER.debug(ret.toDot());
    }

    @Test
    public void testMultiDisjunctionTranslator() {
        ConstraintNetwork ret = CnetworkPreprocessor.INSTANCE
                .translate(cn1);
        LOGGER.debug(ret.toDot());

    }

    @Test
    public void testImplicationTranslator() {
        ConstraintNetwork ret = CnetworkPreprocessor.INSTANCE
                .translate(cn2);
        LOGGER.debug(ret.toDot());
    }

    @Test
    public void testITETranslator() {

        LOGGER.debug(cn3.toDot());
        ConstraintNetwork ret = CnetworkPreprocessor.INSTANCE
                .translate(cn3);
        LOGGER.debug(ret.toDot());
    }

}



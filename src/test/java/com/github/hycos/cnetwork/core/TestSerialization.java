package com.github.hycos.cnetwork.core;

import com.github.hycos.cnetwork.api.labelmgr.LabelManagerInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.*;
import com.github.hycos.cnetwork.tools.io.Serializer;
import com.github.hycos.cnetwork.utils.BiMap;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestSerialization {

    final static Logger LOGGER = LoggerFactory.getLogger(TestSerialization.class);


    private ConstraintNetworkBuilder getBuilder() {

        ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();
        Node a = cb.addOperand(DefaultNodeKind.STRVAR, "bs");
        Node five = cb.addOperand(DefaultNodeKind.NUMLIT, "5");

        try {
            Node len = cb.addOperation(DefaultNodeKind.LEN, a);
            cb.addConstraint(DefaultNodeKind.GREATER, len, five);
            LOGGER.debug("add cons");
        } catch (InconsistencyException e) {
            LOGGER.error("incons " + e.getMessage());
            Assertions.assertFalse(true);
        }
        return cb;
    }

    @Test
    public void testDefaultLabelManager() {


        ConstraintNetworkBuilder cb = getBuilder();

        DefaultLabelManager bm = new DefaultLabelManager();

        for(Node n : cb.vertexSet()) {
            bm.setLabelForNode(n, n.getLabel());
        }

        LOGGER.debug(bm.toString());

        byte [] o = Serializer.INSTANCE.toByteArray(bm);


        DefaultLabelManager b = (DefaultLabelManager)Serializer.INSTANCE.byteArrayToObject(o);

        LOGGER.debug(b.toString());
    }


    @Test
    public void testBidiMap() {

        BiMap bm = new BiMap<Integer, String>();


        bm.put(0, "bs");
        bm.put(1, "5");
        bm.put(2, "len(bs)");
        bm.put(3, ">(len(bs),5)");


        LOGGER.debug(bm.toString());

        byte [] o = Serializer.INSTANCE.toByteArray(bm);


        BiMap b = (BiMap)Serializer.INSTANCE.byteArrayToObject(o);

        LOGGER.debug(b.toString());
    }

    @Test
    public void testDefaultLabelManager2() {


        ConstraintNetwork cn = new ConstraintNetwork();

        Node n1 = new Operand("bs", DefaultNodeKind.STRVAR);
        Node n2 = new Operand("5", DefaultNodeKind.NUMLIT);
        Node n3 = new Operand("len(bs)", DefaultNodeKind.LEN);
        Node n4 = new Operand(">(len(bs),5)", DefaultNodeKind.GREATER);

        DefaultLabelManager lm = new DefaultLabelManager();

        lm.setLabelForNode(n1, "bs");
        lm.setLabelForNode(n2, "5");
        lm.setLabelForNode(n3, "len(bs)");
        lm.setLabelForNode(n4, ">(len(bs),5)");



        byte [] o = Serializer.INSTANCE.toByteArray(lm);


        DefaultLabelManager b = (DefaultLabelManager)Serializer.INSTANCE.byteArrayToObject(o);

        LOGGER.debug(b.toString());
    }


    @Test
    public void testSerializeNetworkBuilder() {


        ConstraintNetworkBuilder cb = getBuilder();

        byte [] b = Serializer.INSTANCE.toByteArray(cb);

        ConstraintNetworkBuilder nn = (ConstraintNetworkBuilder)Serializer.INSTANCE.byteArrayToObject(b);

        Assertions.assertEquals(nn.vertexSet().size(), cb.vertexSet().size());


        for(int i = 0; i < nn.vertexSet().size(); i++) {
            Node n0 = nn.getNodeById(i);
            Node n1 = cb.getNodeById(i);

            LOGGER.debug(" " + i + ":" + n0.getLabel());
            LOGGER.debug(" " + i + ":" + n1.getLabel());
            Assertions.assertEquals(n0.getLabel(), n1.getLabel());

        }
    }



    @Test
    public void testSerializeNetworkBuilder2() {

        ConstraintNetworkBuilder cb = getBuilder();

        byte [] b = Serializer.INSTANCE.toByteArray(cb);


        ConstraintNetworkBuilder nn = (ConstraintNetworkBuilder)Serializer
                .INSTANCE
                .byteArrayToObject(b);

        Assertions.assertEquals(nn.vertexSet(), cb.vertexSet());

        LabelManagerInterface<Node, Edge> n = cb.getLabelManager();


        LOGGER.debug(n.toString());

        byte [] o = Serializer.INSTANCE.toByteArray(n);


        DefaultLabelManager n2 = (DefaultLabelManager)Serializer.INSTANCE
                .byteArrayToObject(o);

        LOGGER.debug(n2.toString());

    }

    @Test
    public void testSerializeNetworkBuilder3() {

        LOGGER.info("start");
        ConstraintNetworkBuilder cb1 = getBuilder();

        ConstraintNetworkBuilder cb2 = new ConstraintNetworkBuilder(cb1);



        LOGGER.debug(cb1.getLabelManager().toString());
        LOGGER.debug(cb2.getLabelManager().toString());



    }


//    @Test
//    public void testWrite() {
//        ConstraintNetworkBuilder cb = getBuilder();
//
//        for(Node n : cb.vertexSet()) {
//            LOGGER.debug("n id:{}, lbl:{}", n.getId(), n.getLabel());
//        }
//
//
//        byte [] b = Serializer.INSTANCE.toByteArray(cb);
//
//        try {
//            FileUtils.writeByteArrayToFile(new File("/tmp/out.cb"), b);
//        } catch (IOException e) {
//            LOGGER.error(e.getMessage());
//            System.exit(-1);
//        }
//    }
//
//    @Test
//    public void testRead() {
//
//        byte [] b = null;
//
//        try {
//            b = FileUtils.readFileToByteArray(new File("/tmp/out.cb"));
//        } catch (IOException e) {
//            LOGGER.error(e.getMessage());
//            System.exit(-1);
//        }
//
//
//        ConstraintNetworkBuilder nn = Serializer.INSTANCE.byteArrayToObject(b);
//
//        for(Node n : nn.vertexSet()) {
//            LOGGER.debug("n id:{}, lbl:{}", n.getId(), n.getLabel());
//        }
//
//
//    }
}

package org.snt.cnetwork.core;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.exception.EUFInconsistencyException;


public class TestConstraintNetworkBuilder {
    final static Logger LOGGER = LoggerFactory.getLogger(TestConstraintNetworkBuilder.class);

    @Test
    public void testBuilder() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();

            String sor = ".*' +[Oo][Rr] +'";
            Node or = new Operand(sor, NodeKind.STRREXP);
            Node v1 = new Operand("sv7", NodeKind.NUMVAR);
            Node toStrV1 = cb.addOperation(NodeKind.TOSTR, v1);
            Node orv1 = cb.addOperation(NodeKind.CONCAT, or, toStrV1);
            Node eq = new Operand(" +\\>= +", NodeKind.STRREXP);
            Node orv1comp = cb.addOperation(NodeKind.CONCAT, orv1, eq);
            Node v2 = new Operand("sv8", NodeKind.NUMVAR);
            Node toStrV2 = cb.addOperation(NodeKind.TOSTR, v2);
            Node orv1compv2 = cb.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);
            String scomment = "(\\<!\\-\\-|#)";
            Node comment = new Operand(scomment, NodeKind.STRREXP);

            cb.addOperation(NodeKind.CONCAT, orv1compv2, comment);

            LOGGER.debug("REMOVE {}", orv1compv2.getId());
            cb.removeVertex(orv1compv2);
            //cb.addConstraint(NodeKind.NUM_EQUALS, v1, v2);
            //cb.addConstraint(NodeKind.MATCHES, x, orv1compv2);
            //cb.addConstraint(NodeKind.GREATER, v1, v3);


            //cb.addConstraint(NodeKind.EQUALS, v3, v2);

            //LOGGER.debug(cb.getConstraintNetwork().toDot());
            LOGGER.debug(cb.getEufLattice().toDot());

        } catch (EUFInconsistencyException e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void testBuilder2() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node zero = cn.addOperand(NodeKind.NUMLIT, "0");
        Node one = cn.addOperand(NodeKind.NUMLIT, "1");
        Node three = cn.addOperand(NodeKind.NUMLIT, "3");
        Node dot = cn.addOperand(NodeKind.STRLIT, ".");

        try {
            Node add = cn.addOperation(NodeKind.ADD, one, zero);
            cn.addConstraint(NodeKind.EQUALS, add, zero);
            //Node subone = cn.addOperation(NodeKind.SUBSTR, idxof, one);
            //Node sub = cn.addOperation(NodeKind.SUBSTR,idxof, subone);
            //cn.addConstraint(NodeKind.NUM_EQUALS, one, sub);
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
        }

        LOGGER.debug(cn.getEufLattice().toDot());


    }


    @Test
    public void testDeleteNode() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node zero = cn.addOperand(NodeKind.NUMLIT, "0");
        Node one = cn.addOperand(NodeKind.NUMLIT, "1");
        Node filename_1 = cn.addOperand(NodeKind.NUMVAR, "filename_1");
        Node dot = cn.addOperand(NodeKind.STRLIT, ".");

        try {
            Node idxof = cn.addOperation(NodeKind.INDEXOF, filename_1, dot, zero);
            cn.addConstraint(NodeKind.EQUALS, idxof, zero);
            Node subone = cn.addOperation(NodeKind.SUBSTR, idxof, one);
            Node sub = cn.addOperation(NodeKind.SUBSTR,idxof, subone);
            cn.addConstraint(NodeKind.NUM_EQUALS, one, sub);

            //cn.removeVertex(subone);
            LOGGER.debug(cn.getConstraintNetwork().toDot());
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
        }

        LOGGER.debug(cn.getEufLattice().toDot());


    }


}

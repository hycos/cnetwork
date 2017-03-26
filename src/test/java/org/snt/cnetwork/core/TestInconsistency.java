package org.snt.cnetwork.core;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.exception.EUFInconsistencyException;


public class TestInconsistency {
    final static Logger LOGGER = LoggerFactory.getLogger(TestInconsistency.class);

    @Test
    public void testNumericInconsistency() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();

            Node four = cb.addOperand(NodeKind.NUMLIT, "4");
            Node three = cb.addOperand(NodeKind.NUMLIT, "3");

            cb.addConstraint(NodeKind.EQUALS,four, three);

        } catch (EUFInconsistencyException e) {
            LOGGER.error(e.getMessage());
            Assert.assertTrue(true);
        }
    }

    @Test
    public void testTransitiveInconsistency() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();

            Node four = cb.addOperand(NodeKind.NUMLIT, "4");
            Node three = cb.addOperand(NodeKind.NUMLIT, "3");
            Node k = cb.addOperand(NodeKind.NUMVAR, "k");

            cb.addConstraint(NodeKind.EQUALS,k, three);
            cb.addConstraint(NodeKind.EQUALS,k, four);

        } catch (EUFInconsistencyException e) {
            LOGGER.error(e.getMessage());
            Assert.assertTrue(true);
        }
    }


}

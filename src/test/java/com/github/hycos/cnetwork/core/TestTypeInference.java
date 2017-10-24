/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetwork.core;

import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TestTypeInference {
    final static Logger LOGGER = LoggerFactory.getLogger(TestTypeInference.class);


    @Test
    public void testNumeric() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();
            Node four = cb.addOperand(DefaultNodeKind.NUMLIT, "4");
            Node k = cb.addOperand(DefaultNodeKind.NUMVAR, "k");
            Node five = cb.addOperand(DefaultNodeKind.NUMLIT, "5");
            Node eq1 = cb.addConstraint(DefaultNodeKind.EQUALS,four, k);
            Assert.assertEquals(eq1.getKind(),DefaultNodeKind.NUM_EQUALS);
//            Node eq2 = cb.addConstraint(DefaultNodeKind.NEQUALS,five, k);
//            Assert.assertEquals(eq2.getKind(),DefaultNodeKind.NUM_NEQUALS);
        } catch (InconsistencyException e) {
            LOGGER.error(e.getMessage());
            Assert.assertTrue(false);
        }
    }

    @Test
    public void testString() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();
            Node four = cb.addOperand(DefaultNodeKind.STRLIT, "4");
            Node k = cb.addOperand(DefaultNodeKind.STRVAR, "k");
            Node five = cb.addOperand(DefaultNodeKind.STRLIT, "5");
            Node eq1 = cb.addConstraint(DefaultNodeKind.EQUALS,four, k);
            Assert.assertEquals(eq1.getKind(),DefaultNodeKind.STR_EQUALS);
            Node eq2 = cb.addConstraint(DefaultNodeKind.NEQUALS,five, k);
            Assert.assertEquals(eq2.getKind(),DefaultNodeKind.STR_NEQUALS);

            LOGGER.info(cb.getConstraintNetwork().toDot());
        } catch (InconsistencyException e) {
            LOGGER.error(e.getMessage());
            Assert.assertTrue(false);
        }
    }

    @Test
    public void testBoolean() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();
            Node t = cb.addOperand(DefaultNodeKind.BOOLLIT, "true");
            Node k = cb.addOperand(DefaultNodeKind.BOOLVAR, "k");
            Node f = cb.addOperand(DefaultNodeKind.BOOLLIT, "false");
            Node eq1 = cb.addConstraint(DefaultNodeKind.EQUALS,t, k);
            Assert.assertEquals(eq1.getKind(),DefaultNodeKind.BOOL_EQUALS);
            Node eq2 = cb.addConstraint(DefaultNodeKind.NEQUALS,f, k);
            Assert.assertEquals(eq2.getKind(),DefaultNodeKind.BOOL_NEQUALS);
        } catch (InconsistencyException e) {
            LOGGER.error(e.getMessage());
            Assert.assertTrue(false);
        }
    }

}

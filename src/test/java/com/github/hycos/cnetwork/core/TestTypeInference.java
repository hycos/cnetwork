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

import com.github.hycos.cnetwork.core.graph.NodeKind;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;


public class TestTypeInference {
    final static Logger LOGGER = LoggerFactory.getLogger(TestTypeInference.class);


    @Test
    public void testNumeric() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();

            Node four = cb.addOperand(NodeKind.NUMLIT, "4");
            Node k = cb.addOperand(NodeKind.NUMVAR, "k");
            Node five = cb.addOperand(NodeKind.NUMLIT, "5");

            Node eq1 = cb.addConstraint(NodeKind.EQUALS,four, k);

            Assert.assertTrue(eq1.getKind() == NodeKind.NUM_EQUALS);

            Node eq2 = cb.addConstraint(NodeKind.NEQUALS,five, k);

            Assert.assertTrue(eq2.getKind() == NodeKind.NUM_NEQUALS);

        } catch (EUFInconsistencyException e) {
            LOGGER.error(e.getMessage());
            Assert.assertTrue(false);
        }
    }

    @Test
    public void testString() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();

            Node four = cb.addOperand(NodeKind.STRLIT, "4");
            Node k = cb.addOperand(NodeKind.STRVAR, "k");
            Node five = cb.addOperand(NodeKind.STRLIT, "5");

            Node eq1 = cb.addConstraint(NodeKind.EQUALS,four, k);

            Assert.assertTrue(eq1.getKind() == NodeKind.STR_EQUALS);

            Node eq2 = cb.addConstraint(NodeKind.NEQUALS,five, k);

            Assert.assertTrue(eq2.getKind() == NodeKind.STR_NEQUALS);

        } catch (EUFInconsistencyException e) {
            LOGGER.error(e.getMessage());
            Assert.assertTrue(false);
        }
    }

    @Test
    public void testBoolean() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();

            Node t = cb.addOperand(NodeKind.BOOLLIT, "true");
            Node k = cb.addOperand(NodeKind.BOOLVAR, "k");
            Node f = cb.addOperand(NodeKind.BOOLLIT, "false");

            Node eq1 = cb.addConstraint(NodeKind.EQUALS,t, k);

            Assert.assertTrue(eq1.getKind() == NodeKind.BOOL_EQUALS);

            Node eq2 = cb.addConstraint(NodeKind.NEQUALS,f, k);

            Assert.assertTrue(eq2.getKind() == NodeKind.BOOL_NEQUALS);

        } catch (EUFInconsistencyException e) {
            LOGGER.error(e.getMessage());
            Assert.assertTrue(false);
        }
    }




}

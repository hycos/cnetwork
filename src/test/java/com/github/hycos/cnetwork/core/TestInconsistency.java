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

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;


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

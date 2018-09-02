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
import com.github.hycos.cnetwork.core.graph.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TestConstraintNetworkGeneration {

    final static Logger LOGGER = LoggerFactory.getLogger(TestConstraintNetworkGeneration.class);


    @Test
    public void testCnConstruction() {

        ConstraintNetworkBuilder tm2 = new ConstraintNetworkBuilder();
        Node x = new Operand("x", DefaultNodeKind.STRVAR);
        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, DefaultNodeKind.STREXP);
        Node v1 = new Operand("sv7", DefaultNodeKind.NUMVAR);
        try {
            buildNetwork(tm2, x, or, v1);
        } catch (InconsistencyException e) {
            Assertions.assertTrue(false);
        }

    }


    @Test
    public void testCNClone() {
        ConstraintNetworkBuilder tm2 = new ConstraintNetworkBuilder();
        Node x = new Operand("x", DefaultNodeKind.STRVAR);
        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, DefaultNodeKind.STREXP);
        Node v1 = new Operand("sv7", DefaultNodeKind.NUMVAR);

        try {
            buildNetwork(tm2, x, or, v1);
        } catch (InconsistencyException e) {
            Assertions.assertFalse(true);
        }

        LOGGER.info(tm2.getConstraintNetwork().toDot());
        ConstraintNetworkBuilder tm3 = tm2.clone();

        Assertions.assertNotNull(tm3.getConstraintNetwork());

        LOGGER.info(tm3.getConstraintNetwork().toDot());


        for (Edge e1 : tm2.edgeSet()) {
            for (Edge e2 : tm3.edgeSet()) {
                Assertions.assertTrue(e1 != e2);
            }
        }

        for (Node n1 : tm2.vertexSet()) {
            for (Node n2 : tm3.vertexSet()) {
                Assertions.assertTrue(n1 != n2);
            }
        }
    }

    private void buildNetwork(ConstraintNetworkBuilder tm2, Node x, Node or, Node v1) throws InconsistencyException {
        Node toStrV1 = tm2.addOperation(DefaultNodeKind.TOSTR, v1);
        Node orv1 = tm2.addOperation(DefaultNodeKind.CONCAT, or, toStrV1);
        Node eq = new Operand(" +\\>= +", DefaultNodeKind.STREXP);
        Node orv1comp = tm2.addOperation(DefaultNodeKind.CONCAT, orv1, eq);
        Node v2 = new Operand("sv8", DefaultNodeKind.NUMVAR);
        Node toStrV2 = tm2.addOperation(DefaultNodeKind.TOSTR, v2);
        Node orv1compv2 = tm2.addOperation(DefaultNodeKind.CONCAT, orv1comp, toStrV2);
        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, DefaultNodeKind.STREXP);
        tm2.addOperation(DefaultNodeKind.CONCAT, orv1compv2, comment);
        tm2.addConstraint(DefaultNodeKind.GREATEREQ, v1, v2);
        tm2.setStartNode(orv1compv2);
        tm2.addConstraint(DefaultNodeKind.MATCHES, x, orv1compv2);
    }


    @Test
    public void testCopyConstructor() {

        ConstraintNetworkBuilder cb1 = new ConstraintNetworkBuilder();

        Node a = cb1.addOperand(DefaultNodeKind.STRVAR, "a");
        Node b = cb1.addOperand(DefaultNodeKind.STRVAR, "b");
        Node c = cb1.addOperand(DefaultNodeKind.STRVAR, "c");

        Node concat1 = null, concat2 = null;

        try {
            concat1 = cb1.addOperation(DefaultNodeKind.CONCAT, a, b);
            concat2 = cb1.addOperation(DefaultNodeKind.CONCAT, concat1, c);
        } catch (InconsistencyException e) {
            e.printStackTrace();
            Assertions.assertFalse(true);
        }

        ConstraintNetworkBuilder cb2 = new ConstraintNetworkBuilder(cb1);

        LOGGER.debug(cb1.getConstraintNetwork().toDot());

        LOGGER.debug(cb2.getConstraintNetwork().toDot());



        LOGGER.debug("l1 " + concat1.getLabel());
        LOGGER.debug("l2 " + concat2.getLabel());


        try {
            Node n = cb2.addConstraint(DefaultNodeKind.STR_EQUALS, concat1, concat2);

            LOGGER.debug("NID {}", n.getId());
        } catch (InconsistencyException e) {
           LOGGER.debug(e.getMessage());
           e.printStackTrace();
           System.exit(-1);
        }

        Assertions.assertEquals(cb1.vertexSet().size(), 5);
        Assertions.assertEquals(cb2.vertexSet().size(), 6);

        Node d = cb1.addOperand(DefaultNodeKind.STRVAR, "d");

        try {
            cb1.addConstraint(DefaultNodeKind.EQUALS, d, d);
        } catch (InconsistencyException e) {
            e.printStackTrace();
        }


        Assertions.assertEquals(cb1.vertexSet().size(), 7);
        Assertions.assertEquals(cb2.vertexSet().size(), 6);


        LOGGER.debug(cb1.getConstraintNetwork().toDot());

    }


    @Test
    public void testDummy() {

        ConstraintNetworkBuilder cb1 = new ConstraintNetworkBuilder();

        Node a = cb1.addOperand(DefaultNodeKind.STRVAR, "a");
        Node b = cb1.addOperand(DefaultNodeKind.STRVAR, "b");
        Node c = cb1.addOperand(DefaultNodeKind.STRVAR, "c");

        Node concat1 = null, concat2 = null;

        try {
            concat1 = cb1.addOperation(DefaultNodeKind.CONCAT, a, b);
            concat2 = cb1.addOperation(DefaultNodeKind.CONCAT, concat1, c);
            cb1.addConstraint(DefaultNodeKind.EQUALS, concat1, concat2);
        } catch (InconsistencyException e) {
            e.printStackTrace();
            Assertions.assertFalse(true);
        }


        LOGGER.debug("=========");

        //LOGGER.debug("d {}", cb1.getEufLattice().debug());


        LOGGER.debug("=========");

    }


}



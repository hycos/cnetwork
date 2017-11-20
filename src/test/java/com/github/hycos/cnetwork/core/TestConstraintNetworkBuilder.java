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
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.Operand;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TestConstraintNetworkBuilder {
    final static Logger LOGGER = LoggerFactory.getLogger(TestConstraintNetworkBuilder.class);

    @Test
    public void testDuplicates() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();
            Node a = cb.addOperand(DefaultNodeKind.STRLIT, "a");
            Node b = cb.addOperand(DefaultNodeKind.STRLIT, "b");
            Node va = cb.addOperand(DefaultNodeKind.STRVAR, "a");
            Node vb = cb.addOperand(DefaultNodeKind.STRVAR, "b");
            Node b2 = cb.addOperand(DefaultNodeKind.STRLIT, "b");
            Node va2 = cb.addOperand(DefaultNodeKind.STRVAR, "a");
            cb.addConstraint(DefaultNodeKind.EQUALS, a, b);
            cb.addConstraint(DefaultNodeKind.EQUALS, va2, b2);
            LOGGER.debug(cb.getConstraintNetwork().toDot());
            Node comp1 = cb.getNodeByLabel(va.getLabel());
            Node comp2 = cb.getNodeByLabel(va2.getLabel());
            Assertions.assertEquals(comp1, comp2);
            Assertions.assertEquals(cb.vertexSet().size(), 8);
        } catch (InconsistencyException e) {
            e.printStackTrace();
            LOGGER.debug(e.getMessage());
            Assertions.assertTrue(true);
        }

    }


    @Test
    public void testBuilder() {

        try {
            ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();
            String sor = ".*' +[Oo][Rr] +'";
            Node or = new Operand(sor, DefaultNodeKind.STREXP);
            Node v1 = new Operand("sv7", DefaultNodeKind.NUMVAR);
            Node toStrV1 = cb.addOperation(DefaultNodeKind.TOSTR, v1);
            Node orv1 = cb.addOperation(DefaultNodeKind.CONCAT, or, toStrV1);
            Node eq = new Operand(" +\\>= +", DefaultNodeKind.STREXP);
            Node orv1comp = cb.addOperation(DefaultNodeKind.CONCAT, orv1, eq);
            Node v2 = new Operand("sv8", DefaultNodeKind.NUMVAR);
            Node toStrV2 = cb.addOperation(DefaultNodeKind.TOSTR, v2);
            Node orv1compv2 = cb.addOperation(DefaultNodeKind.CONCAT, orv1comp, toStrV2);
            String scomment = "(\\<!\\-\\-|#)";
            Node comment = new Operand(scomment, DefaultNodeKind.STREXP);
            cb.addOperation(DefaultNodeKind.CONCAT, orv1compv2, comment);
            Node v3 = new Operand("sv7", DefaultNodeKind.NUMVAR);
            Node v8 = new Operand("sv8", DefaultNodeKind.NUMVAR);
            cb.addConstraint(DefaultNodeKind.EQUALS, v3, v8);
            LOGGER.debug(cb.getConstraintNetwork().toDot());
        } catch (InconsistencyException e) {

            e.printStackTrace();
            LOGGER.error(e.getMessage());
            Assertions.assertTrue(true);
        }


    }

    @Test
    public void testBuilder2() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        try {
            Node zero = cn.addOperand(DefaultNodeKind.NUMLIT, "0");
            Node one = cn.addOperand(DefaultNodeKind.NUMLIT, "1");
            Node three = cn.addOperand(DefaultNodeKind.NUMLIT, "3");
            Node dot = cn.addOperand(DefaultNodeKind.STRLIT, ".");
            Node add = cn.addOperation(DefaultNodeKind.ADD, one, zero);
            cn.addConstraint(DefaultNodeKind.EQUALS, add, zero);
            //Node subone = cn.addOperation(NodeKind.SUBSTR, idxof, one);
            //Node sub = cn.addOperation(NodeKind.SUBSTR,idxof, subone);
            //cn.addConstraint(NodeKind.NUM_EQUALS, one, sub);
        } catch (InconsistencyException e) {
            e.printStackTrace();
            Assertions.assertFalse(true);
        }

        //LOGGER.debug(cn.getEufLattice().toDot());


    }


    @Test
    public void testDeleteNode() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        try {
            Node zero = cn.addOperand(DefaultNodeKind.NUMLIT, "0");
            Node one = cn.addOperand(DefaultNodeKind.NUMLIT, "1");
            Node filename_1 = cn.addOperand(DefaultNodeKind.NUMVAR, "filename_1");
            Node dot = cn.addOperand(DefaultNodeKind.STRLIT, ".");
            Node idxof = cn.addOperation(DefaultNodeKind.INDEXOF, filename_1, dot, zero);
            cn.addConstraint(DefaultNodeKind.EQUALS, idxof, zero);
            Node subone = cn.addOperation(DefaultNodeKind.SUBSTR, dot, idxof,
                    one);
            Node sub = cn.addOperation(DefaultNodeKind.SUBSTR, dot, idxof,
                    subone);
            cn.addConstraint(DefaultNodeKind.NUM_EQUALS, zero, one);
            Assertions.assertEquals(cn.vertexSet().size(), 9);
            cn.removeVertex(subone);
            Assertions.assertEquals(cn.vertexSet().size(), 8);
        } catch (InconsistencyException e) {
            e.printStackTrace();
            Assertions.assertFalse(true);
        }
        //LOGGER.debug(cn.getEufLattice().toDot());
    }


    @Test
    public void testGetNodeByLabel() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        try {
            Node five = cn.addOperand(DefaultNodeKind.NUMLIT, "5");
            Node s = cn.addOperand(DefaultNodeKind.STRLIT, "s");
            Node var = cn.addOperand(DefaultNodeKind.NUMVAR, "v");
            Node tostr = cn.addOperation(DefaultNodeKind.TOSTR, var);
            Node concat = cn.addOperation(DefaultNodeKind.CONCAT, tostr, s);
            LOGGER.debug("s {}", s.getLabel());

            Node s2 = cn.getNodeByLabel("\"s\"");
            Assertions.assertNotNull(s2);
            Assertions.assertEquals(s.getLabel(), s2.getLabel());
        } catch (InconsistencyException e) {
            e.printStackTrace();
            Assertions.assertFalse(true);
        }

    }

}

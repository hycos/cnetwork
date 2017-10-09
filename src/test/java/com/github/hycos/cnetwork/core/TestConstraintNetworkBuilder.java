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
import com.github.hycos.cnetwork.core.graph.Operand;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import com.github.hycos.cnetwork.utils.EscapeUtils;


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

            LOGGER.debug(cb.getExecutionTree().toDot());

        } catch (EUFInconsistencyException e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void testBuilder2() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();



        try {
            Node zero = cn.addOperand(NodeKind.NUMLIT, "0");
            Node one = cn.addOperand(NodeKind.NUMLIT, "1");
            Node three = cn.addOperand(NodeKind.NUMLIT, "3");
            Node dot = cn.addOperand(NodeKind.STRLIT, ".");
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


        try {
            Node zero = cn.addOperand(NodeKind.NUMLIT, "0");
            Node one = cn.addOperand(NodeKind.NUMLIT, "1");
            Node filename_1 = cn.addOperand(NodeKind.NUMVAR, "filename_1");
            Node dot = cn.addOperand(NodeKind.STRLIT, ".");
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


    @Test
    public void testGetNodeByLabel() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();


        try {

            Node five = cn.addOperand(NodeKind.NUMLIT, "5");
            Node s = cn.addOperand(NodeKind.STRLIT, "s");
            Node var = cn.addOperand(NodeKind.NUMVAR, "v");
            Node tostr = cn.addOperation(NodeKind.TOSTR, var);

            Node concat = cn.addOperation(NodeKind.CONCAT, tostr,s);

            Assert.assertEquals(s, cn.getNodeByLabel(EscapeUtils
                    .escapeSpecialCharacters("\"s\"")));

            Assert.assertEquals(concat, cn.getNodeByLabel(EscapeUtils.escapeSpecialCharacters("concat(tostr(v),\"s\")")));

            Assert.assertEquals(five, cn.getNodeByLabel("5"));
        } catch (EUFInconsistencyException e) {
            e.printStackTrace();
        }
    }

}
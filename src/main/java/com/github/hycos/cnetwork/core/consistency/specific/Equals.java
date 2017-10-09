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

package com.github.hycos.cnetwork.core.consistency.specific;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;

import java.util.List;


public class Equals extends ConsistencyChecker {


    final static Logger LOGGER = LoggerFactory.getLogger(Equals.class);

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        //LOGGER.debug("psize {}", params.size());

        if(params.size() != 2)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);

        LOGGER.debug("n {}", n.getKind().getDesc());

        //LOGGER.debug(">>> {},{},{}", n.getKind().getDesc(),
        //        par0.getKind().getDesc(), par1.getKind().getDesc());

        if(!haveEqualDomains(par0,par1) || !n.getKind().isBoolean()) {
            LOGGER.debug("false1");
            return false;
        }

        if(!cb.vertexSet().containsAll(params)) {
            LOGGER.debug("false 2");
            return false;
        }


        //LOGGER.debug("boo");
        if(n.getKind() == NodeKind.BOOL_EQUALS ||
                n.getKind() == NodeKind.BOOL_NEQUALS) {
            return boolEquals(par1, par0, n);
        }

        if(n.getKind() == NodeKind.STR_EQUALS ||
                n.getKind() == NodeKind.STR_NEQUALS ||
                n.getKind() == NodeKind.STR_EQUALSIC ||
                n.getKind() == NodeKind.STR_NEQUALSIC) {
            boolean ret = strEquals(par0, par1, n);

            LOGGER.debug("RET {}", ret);
            return ret;
        }

        if(n.getKind() == NodeKind.NUM_EQUALS ||
                n.getKind() == NodeKind.NUM_NEQUALS) {
            return numEquals(par0, par1, n);
        }

        if(n.getKind() == NodeKind.EQUALS) {
            if(par1.getKind().isString()) {
                n.setKind(NodeKind.STR_EQUALS);
                return strEquals(par0, par1, n);
            } else if (par1.getKind().isNumeric()) {
                n.setKind(NodeKind.NUM_EQUALS);
                return numEquals(par0, par1, n);
            } else if (par1.getKind().isBoolean()) {
                n.setKind(NodeKind.BOOL_EQUALS);
                return boolEquals(par0, par1, n);
            }
        }

        if(n.getKind() == NodeKind.NEQUALS) {
            if(par1.getKind().isString()) {
                n.setKind(NodeKind.STR_NEQUALS);
                return strEquals(par0, par1, n);
            } else if (par1.getKind().isNumeric()) {
                n.setKind(NodeKind.NUM_NEQUALS);
                return numEquals(par0, par1, n);
            } else if (par1.getKind().isBoolean()) {
                n.setKind(NodeKind.BOOL_NEQUALS);
                return boolEquals(par0, par1, n);
            }
        }

        return true;
    }

    private boolean numEquals(Node par0, Node par1, Node n) {
        return par0.getKind().isNumeric() && par1.getKind().isNumeric() &&
                n.getKind().isBoolean();
    }

    private boolean strEquals(Node par0, Node par1, Node n) {

        LOGGER.debug("0 {}, 1{}, kind {}", par0.getKind().isString(),
                par1.isString(), n.getKind().isBoolean());
        return par0.getKind().isString() && par1.getKind().isString() &&
                n.getKind().isBoolean();
    }

    private boolean boolEquals(Node par0, Node par1, Node n) {
        return par0.getKind().isBoolean() && par1.getKind().isBoolean() &&
                n.getKind().isBoolean();
    }

    private boolean haveEqualDomains(Node par0, Node par1) {
        return ((par0.isNumeric() && par1.isNumeric()) ||
                ((par0.isString() || par0.isRegex())
                        && (par1.isString() || par1.isRegex())) ||
                        par0.isBoolean() || par1.isBoolean());


    }

}

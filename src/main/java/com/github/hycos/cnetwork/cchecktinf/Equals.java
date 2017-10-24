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

package com.github.hycos.cnetwork.cchecktinf;

import com.github.hycos.cnetwork.api.NodeInterface;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkInterface;
import com.github.hycos.cnetwork.api.cchecktinf.AbstractConsistencyChecker;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import com.github.hycos.cnetwork.core.graph.Node;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;


public class Equals extends AbstractConsistencyChecker {


    final static Logger LOGGER = LoggerFactory.getLogger(Equals.class);

    @Override
    public boolean check(ConstraintNetworkInterface cb, NodeInterface n) {

        List<Node> params = cb.getParametersFor(n);

        LOGGER.debug("psize {}", params.size());

        if(params.size() != 2)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);

        LOGGER.debug("p0 {}", par0.getKind().toString());
        LOGGER.debug("p1 {}", par1.getKind().toString());

        LOGGER.debug("n {}",haveEqualDomains(par0,par1));

        //LOGGER.debug(">>> {},{},{}", n.getDesc(),
        //        par0.getDesc(), par1.getDesc());


        if(!haveEqualDomains(par0,par1)) {
            LOGGER.debug("false1");
            return false;
        }

        if(!cb.vertexSet().containsAll(params)) {
            LOGGER.debug("false 2");
            return false;
        }


        //LOGGER.debug("boo");
        if(n.getKind() == DefaultNodeKind.BOOL_EQUALS ||
                n.getKind() == DefaultNodeKind.BOOL_NEQUALS) {
            return boolEquals(par1, par0);
        }

        if(n.getKind()== DefaultNodeKind.STR_EQUALS ||
                n.getKind() == DefaultNodeKind.STR_NEQUALS ||
                n.getKind() == DefaultNodeKind.STR_EQUALSIC ||
                n.getKind() == DefaultNodeKind.STR_NEQUALSIC) {
            boolean ret = strEquals(par0, par1);

            LOGGER.debug("RET {}", ret);
            return ret;
        }

        if(n.getKind() == DefaultNodeKind.NUM_EQUALS ||
                n.getKind() == DefaultNodeKind.NUM_NEQUALS) {
            return numEquals(par0, par1);
        }

        if(n.getKind() == DefaultNodeKind.EQUALS) {

            LOGGER.debug(" >> {}", n.getLabel());
            LOGGER.debug(" >> {}", par0.getKind().isNumeric());
            LOGGER.debug(" >> {}", par1.getKind().isNumeric());

            if(par1.getKind().isString()) {
                n.setKind(DefaultNodeKind.STR_EQUALS);
                return strEquals(par0, par1);
            } else if (par1.getKind().isNumeric()) {
                LOGGER.debug(">>> +++");
                n.setKind(DefaultNodeKind.NUM_EQUALS);
                return numEquals(par0, par1);
            } else if (par1.getKind().isBoolean()) {
                n.setKind(DefaultNodeKind.BOOL_EQUALS);
                return boolEquals(par0, par1);
            }
        }

        if(n.getKind() == DefaultNodeKind.NEQUALS) {
            LOGGER.debug("nequals");
            if(par1.getKind().isString()) {
                n.setKind(DefaultNodeKind.STR_NEQUALS);
                return strEquals(par0, par1);
            } else if (par1.getKind().isNumeric()) {
                n.setKind(DefaultNodeKind.NUM_NEQUALS);
                return numEquals(par0, par1);
            } else if (par1.getKind().isBoolean()) {
                n.setKind(DefaultNodeKind.BOOL_NEQUALS);
                return boolEquals(par0, par1);
            }
        }

        return true;
    }

    private boolean numEquals(NodeInterface par0, NodeInterface par1) {
        return par0.getKind().isNumeric() && par1.getKind().isNumeric();
    }

    private boolean strEquals(NodeInterface par0, NodeInterface par1) {
        return par0.getKind().isString() && par1.getKind().isString();
    }

    private boolean boolEquals(NodeInterface par0, NodeInterface par1) {
        return par0.getKind().isBoolean() && par1.getKind().isBoolean();
    }

    private boolean haveEqualDomains(NodeInterface par0, NodeInterface par1) {
        return ((par0.getKind().isNumeric() && par1.getKind().isNumeric()) ||
                ((par0.getKind().isString() || par0.getKind().isRegex())
                        && (par1.getKind().isString() || par1.getKind()
                        .isRegex())) || par0.getKind().isBoolean());
    }

}

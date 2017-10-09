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
import com.github.hycos.cnetwork.core.graph.EdgeKind;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.consistency.ConsistencyChecker;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

import java.util.List;


public class IndexOf extends ConsistencyChecker {

    final static Logger LOGGER = LoggerFactory.getLogger(IndexOf.class);

    @Override
    public boolean check(ConstraintNetworkBuilder cb, Node n) {

        List<Node> params = cb.getParametersFor(n);

        if(params.size() < 2 || params.size() > 4)
            return false;

        Node par0 = params.get(0);
        Node par1 = params.get(1);
        Node par2;

        if(params.size() == 3) {
            par2 = params.get(2);
        } else {
            par2 = cb.addOperand(NodeKind.NUMLIT, "0");
            try {
                cb.addConnection(par2, n, EdgeKind.PAR_IN, 2);
            } catch (EUFInconsistencyException e) {
                assert false;
            }
        }

        LOGGER.debug("idx {}:{}:{}", par0.getKind(),par1.getKind(),par2.getKind());

        return (n.getKind() == NodeKind.INDEXOF &&
                n.getKind().isNumeric() &&
                params != null &&
                !par0.isNumeric() &&
                !par1.isNumeric()) &&
                par2.isNumeric();
    }
}

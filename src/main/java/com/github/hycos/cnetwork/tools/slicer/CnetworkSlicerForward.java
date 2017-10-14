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

package com.github.hycos.cnetwork.tools.slicer;

import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.Node;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by julian on 07/12/2016.
 */
public class CnetworkSlicerForward extends AbstractCnetworkSlicer {


    public CnetworkSlicerForward(ConstraintNetwork cn) {
        super(cn);
    }

    @Override
    public Collection<Node> getNext(Collection<Node> n) {
        Set<Node> ret = new HashSet();
        for(Node v : n) {
            ret.addAll(cn.outgoingEdgesOf(v).stream().map(e -> e.getDestNode())
                    .collect(Collectors.toSet()));
        }
        return ret;
    }

}

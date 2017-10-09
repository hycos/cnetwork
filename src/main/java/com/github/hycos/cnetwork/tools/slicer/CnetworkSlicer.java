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

import java.util.*;

public abstract class CnetworkSlicer implements Slicer {

    protected ConstraintNetwork cn = null;

    public CnetworkSlicer() {}
    public CnetworkSlicer(ConstraintNetwork cn) {
        this.cn = cn;
    }


    public abstract Collection<Node> getNext(Collection<Node> n);

    @Override
    public void setNetwork(ConstraintNetwork cn) {
        this.cn = cn;
    }

    @Override
    public Collection<Node> slice(Collection<Node> criteria) {

        assert this.cn != null;

        Set<Node> bw = new LinkedHashSet<>();
        LinkedList<Node> incoming = new LinkedList();

        incoming.addAll(criteria);

        while(!incoming.isEmpty()) {
            Node e = incoming.pop();
            if(!bw.contains(e)) {
                incoming.addAll(getNext(Collections.singleton(e)));
                bw.add(e);
            }

        }
        return bw;
    }

    @Override
    public Collection<Node> slice(Node criterion) {
        return this.slice(Collections.singleton(criterion));
    }

}

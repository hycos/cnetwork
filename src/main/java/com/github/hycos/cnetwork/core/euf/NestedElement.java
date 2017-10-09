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

package com.github.hycos.cnetwork.core.euf;


import com.github.hycos.cnetwork.core.graph.Node;

import java.util.Arrays;

public final class NestedElement extends Element {

    protected final Element [] tuple;

    /**
     * flat copy of a nested element. Deep copy
     * would be too expensive because of the recursive
     * nature of this data structure
     * @param ne
     */
    public NestedElement(NestedElement ne) {
        this(ne.mappedNode,ne.lbl, ne.annotation, Arrays.copyOf(ne.tuple, ne.tuple
                .length));
    }

    public NestedElement(Node n, String label, String annotation, Element ...
            pars) {
       super(n, label,annotation);
       tuple = new Element[pars.length];

       for(int i = 0; i < pars.length; i++) {
           assert pars[i] != null;
           tuple[i] = pars[i].clone();
       }
    }

    public Element [] getTuples() {
        return tuple;
    }

    public boolean isTuple() {
        return true;
    }

    @Override
    public Element clone() {
        return new NestedElement(this);
    }

    @Override
    public boolean isNested() {
        return true;
    }

    public boolean isSingleton() {
        return false;
    }


    @Override
    public Element [] split() {

       // LOGGER.debug("split");

        //for(Element e : tuple) {
        //    LOGGER.debug(e.getLabel());
       // }
        assert tuple != null;
        return Arrays.copyOf(tuple, tuple.length);
    }

}

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
import com.github.hycos.cnetwork.utils.BiMap;

public class NodeCache {

    private BiMap<Node, EquiClass> ncache = new BiMap<>();
    private BiMap<String, Node> scache = new BiMap<>();

    public NodeCache() {
        ncache = new BiMap<>();
        scache = new BiMap<>();
    }

    public NodeCache(NodeCache other) {
        ncache = new BiMap<>(other.ncache);
        scache = new BiMap<>(other.scache);
    }

    public void put(Node n , EquiClass c) {
        ncache.put(n, c);
        scache.put(c.getLabel(), n);
    }

    public boolean hasEquiClass(Node n) {
        return ncache.containsKey(n);
    }

    public boolean hasNode(String label) {
        return scache.containsKey(label);
    }

    public EquiClass getEquiClass(Node n) {
        if(hasEquiClass(n)) {
            return ncache.getValueByKey(n);
        }
        return null;
    }

    public EquiClass getEquiClass(String s) {
        if(hasNode(s)) {
            Node n = scache.getValueByKey(s);
            return getEquiClass(n);
        }
        return null;
    }

    public boolean hasLabel(Node n) {
        return scache.containsValue(n);
    }

    public String getLabel(Node n) {
        if(hasEquiClass(n)) {
            return ncache.getValueByKey(n).getLabel();
        }
        return null;
    }

    public Node getNode(String lbl) {
        if(hasNode(lbl)){
            scache.getValueByKey(lbl);
        }
        return null;
    }


}

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

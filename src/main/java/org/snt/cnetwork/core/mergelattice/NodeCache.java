package org.snt.cnetwork.core.mergelattice;

import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.utils.BiMap;

import java.util.Map;


public class NodeCache extends BiMap<Node, EquiClass> {

    public NodeCache(){
        super();
    }

    public NodeCache(NodeCache other) {
        super(other);
    }

    @Override
    public String toString() {


        StringBuilder sb = new StringBuilder();

        sb.append("\nkeytoval: =======\n");
        for (Map.Entry<Node,EquiClass> e : keytoval.entrySet()) {
            sb.append("Key " + e.getKey().getLabel() + "\n");
            sb.append(e.getValue().getDotLabel());
            sb.append("\n--------------------\n");
        }
        sb.append("==================\n");
        sb.append("\nvaltokey: =======\n");


        for (Map.Entry<EquiClass,Node> e : valtokey.entrySet()) {
            sb.append("Key " + e.getKey().getDotLabel() + "\n");
            sb.append(e.getValue().getLabel());
            sb.append("\n--------------------\n");
        }

        sb.append("==================\n");
        return sb.toString();

    }

}

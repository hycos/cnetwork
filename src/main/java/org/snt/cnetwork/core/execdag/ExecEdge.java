package org.snt.cnetwork.core.execdag;

import org.jgrapht.graph.DefaultEdge;
import org.snt.cnetwork.core.graph.Node;

/**
 * Created by julian on 27/04/2017.
 */
public class ExecEdge extends DefaultEdge {

    private Node src;
    private Node dst;

    public ExecEdge(Node src, Node dst) {
        this.src = src;
        this.dst = dst;
    }

    @Override
    public Node getSource(){
        return src;
    }

    @Override
    public Node getTarget() {
        return dst;
    }

}

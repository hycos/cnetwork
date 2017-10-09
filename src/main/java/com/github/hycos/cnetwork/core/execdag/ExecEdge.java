package com.github.hycos.cnetwork.core.execdag;

import org.jgrapht.graph.DefaultEdge;
import com.github.hycos.cnetwork.core.graph.Node;

/**
 * Created by julian on 27/04/2017.
 */
public class ExecEdge extends DefaultEdge {

    private Node src;
    private Node dst;
    private int seq;

    public ExecEdge(Node src, Node dst, int seq) {
        this.src = src;
        this.dst = dst;
        this.seq = seq;
    }

    @Override
    public Node getSource(){
        return src;
    }

    @Override
    public Node getTarget() {
        return dst;
    }

    public int getSequence() {
        return this.seq;
    }

}

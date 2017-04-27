package org.snt.cnetwork.tools.slicer;

import org.snt.cnetwork.core.graph.ConstraintNetwork;
import org.snt.cnetwork.core.graph.Node;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;


public class CnetworkSlicerBackward extends CnetworkSlicer {

    public CnetworkSlicerBackward(ConstraintNetwork cn) {
        super(cn);
    }

    @Override
    public Collection<Node> getNext(Collection<Node> n) {
        Set<Node> ret = new HashSet();
        for(Node v : n) {
            ret.addAll(cn.incomingEdgesOf(v).stream().map(e -> e.getSrcNode())
                    .collect(Collectors.toSet()));
        }
        return ret;
    }
}

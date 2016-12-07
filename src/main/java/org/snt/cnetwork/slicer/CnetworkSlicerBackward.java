package org.snt.cnetwork.slicer;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;


public class CnetworkSlicerBackward extends CnetworkSlicer {

    public CnetworkSlicerBackward() {}

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

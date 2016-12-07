package org.snt.cnetwork.slicer;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by julian on 07/12/2016.
 */
public class CnetworkSlicerForward extends CnetworkSlicer {

    public CnetworkSlicerForward() {}

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

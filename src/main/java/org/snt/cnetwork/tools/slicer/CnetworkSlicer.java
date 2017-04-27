package org.snt.cnetwork.tools.slicer;

import org.snt.cnetwork.core.graph.ConstraintNetwork;
import org.snt.cnetwork.core.graph.Node;

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

        Set<Node> bw = new HashSet();
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

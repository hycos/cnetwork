package org.snt.cnetwork.tools.slicer;


import org.snt.cnetwork.core.graph.ConstraintNetwork;
import org.snt.cnetwork.core.graph.Node;

import java.util.Collection;

public interface Slicer {
    void setNetwork(ConstraintNetwork cn);
    Collection<Node> slice(Collection<Node> criteria);
    Collection<Node> slice(Node criterion);
}

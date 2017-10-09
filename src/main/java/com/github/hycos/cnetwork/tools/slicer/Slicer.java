package com.github.hycos.cnetwork.tools.slicer;


import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.Node;

import java.util.Collection;

public interface Slicer {
    void setNetwork(ConstraintNetwork cn);
    Collection<Node> slice(Collection<Node> criteria);
    Collection<Node> slice(Node criterion);
}

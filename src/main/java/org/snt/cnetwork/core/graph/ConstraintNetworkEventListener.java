package org.snt.cnetwork.core.graph;

public interface ConstraintNetworkEventListener {
    void onNodeMerge(Node toReplace, Node replacement);
}

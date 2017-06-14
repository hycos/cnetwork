package org.snt.cnetwork.core.graph;

public interface ConstraintNetworkEventListener {
    void onNodeMerge(Node toReplace, Node replacement);
    void onNodeDelete(Node deleted);
    void onAddConnection(Edge e);
}

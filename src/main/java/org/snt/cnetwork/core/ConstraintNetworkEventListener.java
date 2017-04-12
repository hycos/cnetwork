package org.snt.cnetwork.core;

public interface ConstraintNetworkEventListener {
    void onNodeMerge(Node toReplace, Node replacement);
}

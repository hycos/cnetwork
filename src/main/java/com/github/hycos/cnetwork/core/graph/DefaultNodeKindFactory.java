package com.github.hycos.cnetwork.core.graph;

import com.github.hycos.cnetwork.api.NodeKindFactoryInterface;
import com.github.hycos.cnetwork.api.NodeKindInterface;

public enum DefaultNodeKindFactory implements NodeKindFactoryInterface {
    INSTANCE;
    @Override
    public NodeKindInterface getNodeKindFromString(String s) {
       return DefaultNodeKind.KindFromString(s);
    }
}

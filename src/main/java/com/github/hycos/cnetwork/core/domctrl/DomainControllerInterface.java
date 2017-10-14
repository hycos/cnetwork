package com.github.hycos.cnetwork.core.domctrl;

import com.github.hycos.cnetwork.core.domain.NodeDomain;
import com.github.hycos.cnetwork.core.domctrl.exception.DomainControllerException;
import com.github.hycos.cnetwork.core.graph.Node;

/**
 * control the interaction between the cnetwork and
 * the domain Interface
 */
public interface DomainControllerInterface {
    NodeDomain getDomainForNode(Node n) throws DomainControllerException;
    void setDomainForNode(Node n, NodeDomain dom) throws DomainControllerException;
    boolean hasNode(Node n);



}

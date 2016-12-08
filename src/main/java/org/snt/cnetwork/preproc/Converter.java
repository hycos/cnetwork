package org.snt.cnetwork.preproc;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;


public interface Converter {
    NodeKind getKind();
    void translate(ConstraintNetwork cn, Node n);
}

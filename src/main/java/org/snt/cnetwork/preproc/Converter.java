package org.snt.cnetwork.preproc;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;


public interface Converter {
    boolean match(ConstraintNetwork cn, Node n);
    void translate(ConstraintNetwork cn, Node n);
}

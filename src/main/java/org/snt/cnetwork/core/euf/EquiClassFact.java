package org.snt.cnetwork.core.euf;

import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.exception.MissingItemException;

import java.util.Collection;

public interface EquiClassFact {
    Collection<EquiClass> createEquiClasses(Node... par);
    Collection<EquiClass> createEquiClass(Node p);
    EquiClass[] getEquiClassesFor(Node ... n) throws MissingItemException;
    EquiClass getEquiClassFor(Node n) throws MissingItemException;
    boolean hasEquiClassFor(Node n);
    void relink(Node toReplace, Node replacement);
}

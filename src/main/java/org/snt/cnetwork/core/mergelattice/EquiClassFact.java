package org.snt.cnetwork.core.mergelattice;

import org.snt.cnetwork.exception.MissingItemException;

import java.util.Collection;

public interface EquiClassFact<T> {
    Collection<EquiClass> createEquiClasses(T ... par);
    String computeLabel(Element ... e);
    EquiClass[] getEquiClassesFor(T ... n) throws MissingItemException;
}

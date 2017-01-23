package org.snt.cnetwork.core.mergelattice;

import java.util.Collection;

public interface EquiClassFact<T> {
    Collection<EquiClass> createEquiClasses(T ... par);
    String computeLabel(String ... s);
    String computeLabel(Element ... e);
}

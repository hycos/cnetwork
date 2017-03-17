package org.snt.cnetwork.core.euf;

import org.snt.cnetwork.exception.MissingItemException;

import java.util.Collection;

public interface EquiClassFact<T> {
    Collection<EquiClass> createEquiClasses(T ... par);
    Collection<EquiClass> createEquiClass(T p);
    EquiClass[] getEquiClassesFor(T ... n) throws MissingItemException;
    EquiClass getEquiClassFor(T n) throws MissingItemException;
    boolean hasEquiClassFor(T n);
    void remove(T n);
}

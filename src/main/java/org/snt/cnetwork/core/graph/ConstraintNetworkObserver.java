package org.snt.cnetwork.core.graph;


import org.snt.cnetwork.exception.EUFInconsistencyException;

public interface ConstraintNetworkObserver<T> {
    void update(T n) throws EUFInconsistencyException;
    void attach(T observer);
}

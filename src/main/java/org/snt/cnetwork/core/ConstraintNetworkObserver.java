package org.snt.cnetwork.core;


import org.snt.cnetwork.exception.EUFInconsistencyException;

public abstract class ConstraintNetworkObserver<T> {
    protected ConstraintNetworkSubject subject;
    public abstract void update(T n) throws EUFInconsistencyException;
}

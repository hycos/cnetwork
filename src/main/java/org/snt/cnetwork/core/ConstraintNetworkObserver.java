package org.snt.cnetwork.core;


public abstract class ConstraintNetworkObserver<T> {
    protected ConstraintNetworkSubject subject;
    public abstract void update(T n);
}

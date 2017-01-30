package org.snt.cnetwork.core;

import org.snt.cnetwork.exception.EUFInconsistencyException;

import java.util.HashSet;
import java.util.Set;

public class ConstraintNetworkSubject<T> {

    private Set<ConstraintNetworkObserver<T>> observers = new
            HashSet<>();

    public void attach(ConstraintNetworkObserver observer){
        observers.add(observer);
    }

    public void notifyAllObservers(T item) throws EUFInconsistencyException {
        for (ConstraintNetworkObserver observer : observers) {
            observer.update(item);
        }
    }

}

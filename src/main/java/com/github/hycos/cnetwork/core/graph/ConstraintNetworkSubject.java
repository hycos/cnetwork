package com.github.hycos.cnetwork.core.graph;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

import java.util.HashSet;
import java.util.Set;

public class ConstraintNetworkSubject<T> {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkSubject.class);

    private Set<ConstraintNetworkObserver<T>> observers = new
            HashSet<>();

    public void attach(ConstraintNetworkObserver observer){
        observers.add(observer);
    }

    public void notifyAllObservers(T item) throws EUFInconsistencyException {
        LOGGER.debug("notify all observers {}", observers.size());
        for (ConstraintNetworkObserver observer : observers) {
            observer.update(item);
        }
    }

}

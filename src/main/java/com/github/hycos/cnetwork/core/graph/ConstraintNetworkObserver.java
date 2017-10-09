package com.github.hycos.cnetwork.core.graph;


import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

public interface ConstraintNetworkObserver<T> {
    void update(T n) throws EUFInconsistencyException;
    void attach(T observer);
}

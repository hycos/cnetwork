package org.snt.cnetwork.core.domain;


import org.snt.cnetwork.core.graph.Node;

public interface DomainInterface<T> extends Cloneable {

    // initialization which is invoked whenever
    // the domain is instanciated
    void init(Node n);


    T intersect(T y);
    T union(T y);
    T minus(T y);

    boolean subsumes(T y);
    boolean isSingleton();
    boolean isEmpty();

    T complement();

    String getDomainName();

    @Override
    String toString();

    @Override
    boolean equals(Object o);

    T clone();

}

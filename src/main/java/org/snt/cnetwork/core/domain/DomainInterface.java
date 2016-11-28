package org.snt.cnetwork.core.domain;


public interface DomainInterface<T> extends Cloneable {

    T intersect(T y);
    T union(T y);
    T minus(T y);
    T complement(T y);

    boolean subsumes(T y);
    boolean isSingleton();
    boolean isEmpty();

    String getDomainName();

    @Override
    String toString();

    @Override
    boolean equals(Object o);

    T clone();

}

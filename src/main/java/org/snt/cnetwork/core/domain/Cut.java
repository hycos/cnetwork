package org.snt.cnetwork.core.domain;


import java.io.Serializable;

abstract class Cut<C extends Comparable> implements Comparable<Cut<C>>,
        Serializable {

    protected C endpoint;

    protected Cut(Cut<C> c) {
        this.endpoint = c.endpoint;
    }

    protected Cut(C c) {
        this.endpoint = c;
    }

    protected Cut() {
    }


    public boolean isSmallerThan(Cut<C> value) {
        return isSmallerThan(value.endpoint);
    }
    public boolean isSmallerEqualsThan(Cut<C> value) {
        return isSmallerEqualsThan(value.endpoint);
    }
    public boolean isGreaterThan(Cut<C> value){
        return isGreaterThan(value.endpoint);
    }
    public boolean isGreaterEqualsThan(Cut<C> value){
        return isGreaterEqualsThan(value.endpoint);
    }


    public abstract boolean isSmallerThan(C value);
    public abstract boolean isSmallerEqualsThan(C value);
    public abstract boolean isGreaterThan(C value);
    public abstract boolean isGreaterEqualsThan(C value);
    public abstract boolean isAboveAll();
    public abstract boolean isBelowAll();

    public abstract Cut<C> sub(Cut<C> val);
    public abstract Cut<C> add(Cut<C> val);
    public abstract Cut<C> div(Cut<C> val);
    public abstract Cut<C> mul(Cut<C> val);
    public abstract Cut<C> diff(Cut<C> val);
    public abstract Cut<C> negate();

    @Override
    public int hashCode() {
        return toString().hashCode();
    }
    // note: overriden by {BELOW,ABOVE}_ALL
    @Override
    public int compareTo(Cut<C> o) {
        if (isGreaterThan(o)) {
            return 1;
        }
        if (isSmallerThan(o)) {
            return -1;
        }
        return 0;
    }


    public C getEndpoint() {
        return endpoint;
    }

    @Override
    public abstract boolean equals(Object obj);

    abstract boolean isFixed();

}

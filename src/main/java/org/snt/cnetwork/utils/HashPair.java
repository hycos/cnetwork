package org.snt.cnetwork.utils;

public class HashPair<T,K> extends Pair<T,K> {


    public HashPair(T first, K second) {
        super(first, second);
    }

    @Override
    public int hashCode() {
        String s = getFirst().toString() + "::" + getSecond().toString();
        return s.hashCode();
    }

}

package org.snt.cnetwork.core.euf;


import org.snt.cnetwork.core.Node;

public class SingletonElement extends Element {

    public SingletonElement(Node n, String label, String annotation) {
        super(n, label, annotation);
    }

    public SingletonElement(Node n, String label) {
        super(n, label, "");
    }



    public SingletonElement(SingletonElement se) {
        this(se.mappedNode, se.lbl, se.annotation);
    }

    @Override
    public Element[] split() {
        Element [] e = new Element[1];
        e[0] = new SingletonElement(this);
        return e;
    }

    @Override
    public Element clone() {
        return new SingletonElement(this);
    }

    @Override
    public boolean isNested() {
        return false;
    }

    public boolean isTuple() {
        return false;
    }

    public boolean isSingleton() {
        return true;
    }

}

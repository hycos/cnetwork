package org.snt.cnetwork.core.mergelattice;


public class SingletonElement extends Element {

    public SingletonElement(String label, String annotation) {
        super(label, annotation);
    }

    public SingletonElement(String label) {
        super(label, "");
    }



    public SingletonElement(SingletonElement se) {
        this(se.lbl, se.annotation);
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

package org.snt.cnetwork.core.mergelattice;


public class ElementSingleton extends Element {

    public ElementSingleton(String label) {
        super(label);
    }

    @Override
    public Element[] split() {
        Element [] e = new Element[1];
        e[0] = this;
        return e;
    }

    @Override
    public String getLabel() {
        return lbl;
    }

    @Override
    public Element clone() {
        return new ElementSingleton(lbl);
    }

    public boolean isTuple() {
        return false;
    }

    public boolean isSingleton() {
        return true;
    }

}

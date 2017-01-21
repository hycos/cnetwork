package org.snt.cnetwork.core.mergelattice;


public class ElementSingleton extends Element {

    public ElementSingleton(String label) {
        super(label);
    }

    @Override
    public Element[] split() {
        Element [] e = new Element[0];
        e[0] = this;
        return e;
    }

    @Override
    public String getLabel() {
        return lbl;
    }

    public boolean isTuple() {
        return false;
    }

    public boolean isSingleton() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if(!(o instanceof ElementSingleton))
            return false;

        Element e = (Element)o;

        return e.getLabel().equals(this.getLabel());
    }

    @Override
    public int compareTo(Object o) {
        if(!(o instanceof ElementSingleton))
            return -1;

        return getLabel().compareTo(((ElementSingleton)o).getLabel());
    }


}

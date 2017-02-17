package org.snt.cnetwork.core.mergelattice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Element implements Comparable, Cloneable{

    final static Logger LOGGER = LoggerFactory.getLogger(Element.class);

    final protected String lbl;
    protected String annotation = "";

    public Element(String lbl, String annotation) {
        this.lbl = lbl;
        this.annotation = annotation;
    }

    public Element(Element e) {
        this(e.lbl, e.annotation);
    }

    public abstract Element [] split();

    public abstract boolean isTuple();

    public String getLabel() {
        return lbl;
    }

    public String getAnnotation() { return annotation ;}

    public void setAnnotation(String annotation) {
        this.annotation = annotation;
    }


    @Override
    public int hashCode() {
        return getLabel().hashCode();
    }

    @Override
    public String toString() {
        return getLabel();
    }

    @Override
    public boolean equals(Object o) {
        if(!(o instanceof Element))
            return false;

        Element e = (Element)o;

        return e.getLabel().equals(this.getLabel());
    }

    @Override
    public int compareTo(Object o) {
        if(!(o instanceof Element))
            return -1;

        return getLabel().compareTo(((Element)o).getLabel());
    }

    @Override
    public abstract Element clone();

    public abstract boolean isNested();


}

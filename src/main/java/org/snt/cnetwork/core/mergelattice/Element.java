package org.snt.cnetwork.core.mergelattice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Element implements Comparable {

    final static Logger LOGGER = LoggerFactory.getLogger(Element.class);

    protected String lbl;

    public Element(String lbl) {
        this.lbl = lbl;
    }

    public abstract Element [] split();

    public abstract boolean isTuple();

    public abstract String getLabel();


    @Override
    public int hashCode() {
        return getLabel().hashCode();
    }

    @Override
    public String toString() {
        return getLabel();
    }


}

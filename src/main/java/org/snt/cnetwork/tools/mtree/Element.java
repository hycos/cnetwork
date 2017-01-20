package org.snt.cnetwork.tools.mtree;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;

public abstract class Element {

    final static Logger LOGGER = LoggerFactory.getLogger(Element.class);

    public abstract String getLabel();

    public abstract Collection<? extends Element> split();

    public abstract boolean isSplittable();

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

        LOGGER.debug("eq");
        return e.getLabel().equals(this.getLabel());
    }
}

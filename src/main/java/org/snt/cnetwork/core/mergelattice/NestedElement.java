package org.snt.cnetwork.core.mergelattice;


import java.util.*;

public final class NestedElement<T> extends Element<T> {

    protected final Element [] tuple;

    /**
     * flat copy of a nested element. Deep copy
     * would be too expensive because of the recursive
     * nature of this data structure
     * @param ne
     */
    public NestedElement(NestedElement ne) {
        this((T)ne.emap,ne.lbl, ne.annotation, Arrays.copyOf(ne.tuple, ne.tuple
                .length));
    }

    public NestedElement(T n, String label, String annotation, Element ... pars) {
       super(n, label,annotation);
       tuple = new Element[pars.length];

       for(int i = 0; i < pars.length; i++) {
           assert pars[i] != null;
           tuple[i] = pars[i].clone();
       }
    }

    public Element [] getTuples() {
        return tuple;
    }

    public boolean isTuple() {
        return true;
    }

    @Override
    public Element clone() {
        return new NestedElement(this);
    }

    @Override
    public boolean isNested() {
        return true;
    }

    public boolean isSingleton() {
        return false;
    }


    @Override
    public Element [] split() {

        LOGGER.debug("split");

        for(Element e : tuple) {
            LOGGER.debug(e.getLabel());
        }
        assert tuple != null;
        return Arrays.copyOf(tuple, tuple.length);
    }




}

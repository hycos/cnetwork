package org.snt.cnetwork.core.mergelattice;


import java.util.*;

public class NestedElement extends Element {

    protected Element [] tuple = null;

    public NestedElement(String label, Element ... pars) {
       super(label);
       tuple = new Element[pars.length];

       for(int i = 0; i < pars.length; i++) {

           assert pars[i] != null;
           tuple[i] = pars[i];
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
        NestedElement et = new NestedElement(lbl);
        et.tuple = Arrays.copyOf(this.tuple, this.tuple.length);
        return et;
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

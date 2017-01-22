package org.snt.cnetwork.core.mergelattice;


import java.util.*;

public class ElementTuple extends Element {

    protected Element [] tuple = null;

    public ElementTuple(String label, String ... pars) {
       super(label);
       tuple = new Element[pars.length];

       for(int i = 0; i < pars.length; i++) {
           tuple[i] = new ElementSingleton(pars[i]);
       }
    }


    public boolean isTuple() {
        return true;
    }

    @Override
    public Element clone() {
        ElementTuple et = new ElementTuple(lbl);
        et.tuple = Arrays.copyOf(this.tuple, this.tuple.length);
        return et;
    }

    public boolean isSingleton() {
        return false;
    }


    @Override
    public Element [] split() {
        return Arrays.copyOf(tuple, tuple.length);
    }




}

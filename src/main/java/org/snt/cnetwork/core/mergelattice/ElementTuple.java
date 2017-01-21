package org.snt.cnetwork.core.mergelattice;


import java.util.*;

public class ElementTuple extends Element {

    protected Element [] tuple = null;

    public ElementTuple(String label, String ... pars) {
       super(label);
       tuple = new Element[pars.length];

       for(int i = 0; i < pars.length; i++) {
           tuple[0] = new ElementSingleton(pars[0]);
       }
    }


    public boolean isTuple() {
        return true;
    }

    public boolean isSingleton() {
        return false;
    }

    @Override
    public String getLabel() {
        return lbl;
    }



    @Override
    public Element [] split() {
        return Arrays.copyOf(tuple, tuple.length);
    }

    @Override
    public boolean equals(Object o) {
        if(!(o instanceof ElementTuple))
            return false;

        Element e = (Element)o;

        return e.getLabel().equals(this.getLabel());
    }

    @Override
    public int compareTo(Object o) {
        if(!(o instanceof ElementTuple))
            return -1;

        return getLabel().compareTo(((ElementTuple)o).getLabel());
    }


}

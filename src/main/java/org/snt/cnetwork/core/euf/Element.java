package org.snt.cnetwork.core.euf;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.utils.EscapeUtils;

public abstract class Element implements Comparable, Cloneable{

    final static Logger LOGGER = LoggerFactory.getLogger(Element.class);

    protected String lbl = "";
    protected String annotation = "";

    // used to map a 'real' object to this element
    protected Node mappedNode;

    public void setMappedNode(Node mappedNode) {
        this.mappedNode = mappedNode;
    }

    public Element(Node emap, String lbl, String annotation) {
        this.lbl = lbl;
        this.annotation = annotation;

        this.mappedNode = emap;
    }

    public Element(Element e) {
        this(e.mappedNode, e.lbl, e.annotation);
    }

    public abstract Element [] split();

    public abstract boolean isTuple();

    public abstract boolean isSingleton();

    public String getLabel() {
        return lbl;
    }

    public String getDotLabel() {
        return EscapeUtils.escapeSpecialCharacters(this.lbl);
    }

    public String getAnnotation() { return annotation ;}

    public Node getMappedNode() {
        return mappedNode;
    }

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

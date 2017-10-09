/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetwork.core.euf;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.utils.EscapeUtils;

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
        //LOGGER.debug("element lbl {}", lbl);
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

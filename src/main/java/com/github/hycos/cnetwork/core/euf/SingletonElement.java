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


import com.github.hycos.cnetwork.core.graph.Node;

public class SingletonElement extends Element {

    public SingletonElement(Node n, String label, String annotation) {
        super(n, label, annotation);
    }

    public SingletonElement(Node n, String label) {
        super(n, label, "");
    }



    public SingletonElement(SingletonElement se) {
        this(se.mappedNode, se.lbl, se.annotation);
    }

    @Override
    public Element[] split() {
        Element [] e = new Element[1];
        e[0] = new SingletonElement(this);
        return e;
    }

    @Override
    public Element clone() {
        return new SingletonElement(this);
    }

    @Override
    public boolean isNested() {
        return false;
    }

    public boolean isTuple() {
        return false;
    }

    public boolean isSingleton() {
        return true;
    }

}

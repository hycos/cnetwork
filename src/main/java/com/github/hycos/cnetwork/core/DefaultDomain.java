/*
 * polyglot - translate constraints in between different formats
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * polyglot is licensed under the EUPL, Version 1.1 or – as soon
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

package com.github.hycos.cnetwork.core;

import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.domctrl.Domain;
import com.github.hycos.domctrl.DomainKind;
import com.github.hycos.domctrl.SubDomainInterface;

import java.util.Collection;
import java.util.Set;

public class DefaultDomain implements Domain {

    private static final long serialVersionUID = -8834622790754711134L;

    private boolean isConstraint = false;
    private Node n = null;

    public DefaultDomain(Node n, boolean isConstraint) {
        this.n = n;
        this.isConstraint = isConstraint;
    }

    public DefaultDomain(DefaultDomain d) {
        this(d.n, d.isConstraint);
    }

    @Override
    public boolean isAlwaysTrue() {
        return isConstraint;
    }

    @Override
    public boolean isAlwaysFalse() {
        return false;
    }

    @Override
    public void taint(Collection<Integer> id) {

    }

    @Override
    public Set<Integer> getTaints() {
        return null;
    }

    @Override
    public void setTrue() throws InconsistencyException {
        isConstraint = true;
    }

    @Override
    public void setFalse() throws InconsistencyException {

    }

    @Override
    public Domain intersect(Domain other) {
        return null;
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public SubDomainInterface getSubDomain(String subdomain) {
        return null;
    }

    @Override
    public void setSubDomain(SubDomainInterface s) {

    }

    @Override
    public boolean isLiteral() {
        return this.n.getKind().isLiteral();
    }

    @Override
    public boolean isRegex() {
        return this.n.getKind().isRegex();
    }

    @Override
    public boolean isString() {
        return n.getKind().isString();
    }

    @Override
    public boolean isNumeric() {
        return n.getKind().isNumeric();
    }

    @Override
    public boolean isBoolean() {
        return n.getKind().isBoolean();
    }

    @Override
    public boolean isVariable() {
        return n.getKind().isVariable();
    }

    @Override
    public boolean isConstraint() {
        return this.isConstraint;
    }

    @Override
    public boolean isNegative() {
        if(n.getKind().isNumeric() && n.getKind().isLiteral()) {
            return Integer.parseInt(n.getShortLabel()) < 0;
        }

        return false;
    }

    @Override
    public String getLabel() {

        if(isConstraint)
            return "true";
        else
            return "�";
    }

    @Override
    public Domain clone() {
        return new DefaultDomain(this);
    }

    @Override
    public DomainKind getKind() {
        return DomainKind.UNKNOWN;
    }
}

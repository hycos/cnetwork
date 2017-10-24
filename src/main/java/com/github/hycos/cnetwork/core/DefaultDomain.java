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

import com.github.hycos.cnetwork.api.domctrl.Domain;
import com.github.hycos.cnetwork.api.domctrl.DomainKind;
import com.github.hycos.cnetwork.api.domctrl.SubDomainInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;

public class DefaultDomain implements Domain {

    private boolean isConstraint = false;

    public DefaultDomain(boolean isConstraint) {
        this.isConstraint = isConstraint;
    }

    @Override
    public boolean isAlwaysTrue() {
        return false;
    }

    @Override
    public boolean isAlwaysFalse() {
        return false;
    }

    @Override
    public void setTrue() throws InconsistencyException {

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
    public boolean isLiteral() {
        return false;
    }

    @Override
    public boolean isRegex() {
        return false;
    }

    @Override
    public boolean isString() {
        return false;
    }

    @Override
    public boolean isNumeric() {
        return false;
    }

    @Override
    public boolean isBoolean() {
        return false;
    }

    @Override
    public boolean isVariable() {
        return false;
    }

    @Override
    public boolean isConstraint() {
        return isConstraint;
    }

    @Override
    public String getDotLabel() {
        return null;
    }

    @Override
    public Domain clone() {
        return null;
    }

    @Override
    public DomainKind getKind() {
        return null;
    }
}

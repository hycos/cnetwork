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

package com.github.hycos.cnetwork.core.graph;

import com.github.hycos.cnetwork.core.domain.DomainKind;
import com.github.hycos.cnetwork.core.domain.range.BooleanRange;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import com.github.hycos.cnetwork.sig.JavaMethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.domain.NodeDomainFactory;
import com.github.hycos.cnetwork.utils.StandardWrappers;


public class Operation extends Node {

    final static Logger LOGGER = LoggerFactory.getLogger(Operation.class);

    private JavaMethodSignature sig;

    public Operation(Operation op) {
        super(op);
        this.sig = op.getSig();
    }

    public Operation(NodeKind kind) {
        super(kind.toString(), kind);
        LOGGER.debug("Node kind {}", kind);
        assert kind.isOperation() || kind.isComparative() || kind.isBranch();
        this.sig = StandardWrappers.getSigForOperation(this.kind);
        assert(this.sig != null);
    }

    public Operation(String name, JavaMethodSignature sig) {
        super(name, NodeKind.EXTERNAL);

        switch(sig.getReturnType().toBCString()) {
            case "Ljava/lang/String;":
                try {
                    setDomain(NodeDomainFactory.INSTANCE.getDomainForKind(NodeKind.STRVAR));
                } catch (EUFInconsistencyException e) {
                    assert false;
                }
                break;
            case "Z":
                try {
                    setDomain(NodeDomainFactory.INSTANCE.getDomainForKind(NodeKind.BOOLVAR));
                } catch (EUFInconsistencyException e) {
                    assert false;
                }
                break;
            case "I":
                try {
                    setDomain(NodeDomainFactory.INSTANCE.getDomainForKind(NodeKind.NUMVAR));
                } catch (EUFInconsistencyException e) {
                    assert false;
                }
                break;
            default:
                assert(false); // shouldn't happen
                break;
        }
        this.sig = sig;
    }


    @Override
    public boolean isLiteral() {
        return this.getKind().isLiteral();
    }

    @Override
    public boolean isRegex() {
        return false;
    }

    @Override
    public boolean isNumeric() {
        return kind.getDomainKind() == DomainKind.NUMERIC_N || kind
                .getDomainKind() == DomainKind.NUMERIC_NM1 || kind
                .getDomainKind() == DomainKind.NUMERIC_Z;
    }

    @Override
    public boolean isBoolean() {
        return this.kind.getDomainKind() == DomainKind.BOOLEAN;
    }


    @Override
    public boolean isVariable() {
        return false;
    }

    @Override
    public boolean isConstraint() {
        return this.isBoolean() && !((BooleanRange)getRange()).isCatState();
    }

    @Override
    public boolean isString() {
        return kind.getDomainKind().isString();
    }


    @Override
    public boolean isOperation() {
        return true;
    }

    @Override
    public boolean isOperand() {
        return false;
    }


    public JavaMethodSignature getSig() {
        return this.sig;
    }

    @Override
    public Operation clone() { return new Operation(this); };

    @Override
    public void setKind(NodeKind kind) {
        super.setKind(kind);
        this.sig = StandardWrappers.getSigForOperation(this.kind);
    }


}

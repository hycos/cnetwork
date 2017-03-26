package org.snt.cnetwork.core;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.BooleanRange;
import org.snt.cnetwork.core.domain.DomainKind;
import org.snt.cnetwork.core.domain.NodeDomainFactory;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.sig.JavaMethodSignature;
import org.snt.cnetwork.utils.StandardWrappers;


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
                    setDomain(NodeDomainFactory.INSTANCE.getDomain
                            (NodeKind.STRVAR));
                } catch (EUFInconsistencyException e) {
                    assert false;
                }
                break;
            case "Z":
                try {
                    setDomain(NodeDomainFactory.INSTANCE.getDomain
                            (NodeKind.BOOLVAR));
                } catch (EUFInconsistencyException e) {
                    assert false;
                }
                break;
            case "I":
                try {
                    setDomain(NodeDomainFactory.INSTANCE.getDomain
                            (NodeKind.NUMVAR));
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

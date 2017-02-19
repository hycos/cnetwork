package org.snt.cnetwork.core;

import org.snt.cnetwork.core.domain.NodeDomainFactory;
import org.snt.cnetwork.exception.EUFInconsistencyException;

public class Constraint extends Operation {

    public Constraint(String name, String kind) {
        super(name, kind);
    }

    public Constraint(String name, NodeKind kind) {
        super(name,kind);
        assert kind.isComparative();
        try {
            setDomain(NodeDomainFactory.INSTANCE.getDomain(NodeKind
                    .BOOLLIT, "true"));
        } catch (EUFInconsistencyException e) {
            assert false;
        }
        assert(this.isBoolean());
    }

    public Constraint(Constraint c) {
        super(c);
        try {
            setDomain(NodeDomainFactory.INSTANCE.getDomain(NodeKind
                    .BOOLLIT, "true"));
        } catch (EUFInconsistencyException e) {
            assert false;
        }
        assert(this.isBoolean());
    }

    @Override
    public Operation clone() { return new Constraint(this); }

}

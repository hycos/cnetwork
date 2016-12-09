package org.snt.cnetwork.core;

import org.snt.cnetwork.core.domain.NodeDomainFactory;

public class Constraint extends Operation {

    public Constraint(String name, String kind) {
        super(name, kind);
    }

    public Constraint(String name, NodeKind kind) {
        super(name,kind);
        assert kind.isComparative();
        setDomain(NodeDomainFactory.INSTANCE.getDomain(NodeKind
                .BOOLLIT, "true"));
        assert(this.isBoolean());
    }

    public Constraint(Constraint c) {
        super(c);
        setDomain(NodeDomainFactory.INSTANCE.getDomain(NodeKind
                .BOOLLIT, "true"));
        assert(this.isBoolean());
    }

    @Override
    public Operation clone() { return new Constraint(this); }

}

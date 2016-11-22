package org.snt.cnetwork.core;

public class Constraint extends Operation {

    public Constraint(String name, String kind) {
        super(name, kind);
    }

    public Constraint(String name, NodeKind kind) {
        super(name,kind);
        assert kind.isComparative();
        this.dom = NodeDomainFactory.getInstance().getDomain(NodeKind
                .BOOLLIT, "true");
        assert(this.isBoolean());
    }

    public Constraint(Constraint c) {
        super(c);
        this.dom = NodeDomainFactory.getInstance().getDomain(NodeKind
                .BOOLLIT, "true");
        assert(this.isBoolean());
    }

    @Override
    public boolean isConstraint() {
        return true;
    }

    @Override
    public Operation clone() { return new Constraint(this); }

}

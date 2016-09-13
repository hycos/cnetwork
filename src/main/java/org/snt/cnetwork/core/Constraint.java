package org.snt.cnetwork.core;

import org.snt.cnetwork.core.range.BooleanRange;

public class Constraint extends Operation {

    public Constraint(String name, String kind) {
        this(name, OperationKind.KindFromString(kind));
    }

    public Constraint(String name, OperationKind kind) {
        super(name,kind);
        this.setRange(new BooleanRange(BooleanRange.BooleanValue.TRUE));
        assert(this.isBoolean());
    }

    public Constraint(Constraint c) {
        super(c);
        this.setRange(new BooleanRange(BooleanRange.BooleanValue.TRUE));
        assert(this.isBoolean());
    }

    @Override
    public boolean isConstraint() {
        return true;
    }

    @Override
    public Operation clone() { return new Constraint(this); }

}

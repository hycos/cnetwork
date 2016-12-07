package org.snt.cnetwork.core;


public class Operand extends Node {

    public Operand(Operand o) {
        super(o);
    }

    public Operand(String label, String kind) {
       this(label, NodeKind.KindFromString(kind));
    }

    public Operand(String label, NodeKind kind) {
        super(label,kind);
    }

    @Override
    public boolean isOperation() {
        return false;
    }

    @Override
    public boolean isOperand() {
        return true;
    }

    @Override
    public boolean isLiteral() {
        return this.kind.isLiteral();
    }

    @Override
    public boolean isRegex() {
        return this.kind.isRegex();
    }

    @Override
    public boolean isString() {
        return this.kind.isString();
    }

    @Override
    public boolean isNumeric() {
        return this.kind.isNumeric();
    }

    @Override
    public boolean isBoolean() {
        return this.kind.isBoolean();
    }

    @Override
    public boolean isVariable() {
        return !this.kind.isLiteral() && !this.kind.isRegex();
    }

    @Override
    public boolean isConstraint() {
        return false;
    }


    public String getName() {
        return this.label;
    }

    @Override
    public Operand clone() { return new Operand(this); }

}

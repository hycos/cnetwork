package org.snt.cnetwork.core;

import dk.brics.automaton.RegExp;
import org.snt.cnetwork.core.range.BasicRange;
import org.snt.cnetwork.core.range.BooleanRange;
import org.snt.cnetwork.utils.AutomatonUtils;
import org.snt.cnetwork.core.range.NumRange;


public class Operand extends Node {

    protected OperandKind kind;
    protected String label;

    public Operand(Operand o) {
        super(o);
        this.label = o.label;
        this.kind = o.kind;
    }

    public Operand(String label, String kind) {
       this(label, OperandKind.KindFromString(kind));
    }

    public Operand(String label, OperandKind kind) {

        //LOGGER.info("LABEL " + label);

        this.label = label;
        this.kind = kind;

        if(this.getKind() == OperandKind.STRLIT) {
            LOGGER.info("create strlit with " + label);
            this.automaton = new RegExp(label).toAutomaton();
            this.range = new NumRange(AutomatonUtils.getApproxLenRange(this.automaton));
            assert(this.range.getDiff() == 0);
            assert(AutomatonUtils.isLiteral(this.automaton));
            this.label = "\"" + this.label + "\"";
        } else if (this.getKind() == OperandKind.STRREXP) {
            this.automaton = new dk.brics.automaton.RegExp(this.label).toAutomaton();
            this.range = new NumRange(AutomatonUtils.getApproxLenRange(this.automaton));
            //this.label = this.label;
        } else if (this.getKind() == OperandKind.STRVAR) {
            this.automaton = new RegExp(STR_REXP).toAutomaton();
            this.range = new NumRange(AutomatonUtils.getApproxLenRange(this.automaton));
        } else if (this.getKind() == OperandKind.NUMLIT) {
            int value = Integer.parseInt(this.label);
            this.range = new NumRange(new BasicRange(value));
            this.automaton = new RegExp(label).toAutomaton();
        } else if (this.getKind() == OperandKind.NUMVAR) {
            // take the most generic rule for string variables
            this.automaton = new RegExp(Z_REXP).toAutomaton();
            this.range = NumRange.Z.clone();
        } else if (this.getKind() == OperandKind.BOOLLIT) {
            // take the most generic rule for string variables
            if(this.label.equals("true")) {
                this.range = new BooleanRange(BooleanRange.BooleanValue.TRUE);
                this.automaton = new RegExp(String.valueOf(BooleanRange.BooleanValue.TRUE.getId())).toAutomaton();
            } else {
                this.range = new BooleanRange(BooleanRange.BooleanValue.FALSE);
                this.automaton = new RegExp(String.valueOf(BooleanRange.BooleanValue.FALSE.getId())).toAutomaton();
            }
        } else if (this.getKind() == OperandKind.BOOLVAR) {
            // take the most generic rule for string variables
            this.automaton = new RegExp(BOOL_REXP).toAutomaton();
            this.range = new BooleanRange();
        }

    }

    @Override
    public int getKindId() {
        return this.getKind().getId();
    }

    public OperandKind getKind() {
        return kind;
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

    @Override
    public String toString(){ return "{" + this.id + "} " + this.label + "\n" + this.getRange().toString();  }


    @Override
    public String getLabel() {
       //if(this.isString() && this.isLiteral())
       //    return "\"" + this.label + "\"";
       //else
           return this.label;
    }

    @Override
    public void setLabel(String label) {
        this.label = label;
    }

    @Override
    public void setKind(NetworkEntityKind kind) {
        assert(kind instanceof OperandKind);
        this.kind = (OperandKind)kind;
    }

    public String getName() {
        return this.label;
    }

    @Override
    public Operand clone() { return new Operand(this); }
}

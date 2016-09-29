package org.snt.cnetwork.core;

import dk.brics.automaton.RegExp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.range.BooleanRange;
import org.snt.cnetwork.core.range.NumRange;
import org.snt.cnetwork.sig.JavaMethodSignature;
import org.snt.cnetwork.utils.AutomatonUtils;
import org.snt.cnetwork.utils.StandardWrappers;


public class Operation extends Node {

    final static Logger LOGGER = LoggerFactory.getLogger(Operation.class);
    

    private OperationKind kind;
    private String label;
    private JavaMethodSignature sig;

    public Operation(Operation op) {
        super(op);
        this.label = op.label;
        this.kind = op.kind;
        // no need to make full copy of that object
        this.sig = op.sig;
        this.automaton = op.automaton.clone();

        if(op.isBoolean())
            this.range = new BooleanRange((BooleanRange)op.getRange());
        else
            this.range = op.getRange().clone();
    }

    public Operation(String name, String kind) {
        this(name,OperationKind.KindFromString(kind));
    }

    public Operation(String name, OperationKind kind) {
        this.label = name;
        this.kind = kind;
        this.sig = StandardWrappers.getSigForOperation(this.kind);
        init();
    }

    public Operation(String name, JavaMethodSignature sig) {
        this.label = name;
        this.kind = OperationKind.EXTERNAL;

        switch(sig.getReturnType().toBCString()) {
            case "Ljava/lang/String;":
                this.getKind().setReturnType(OperationReturnType.STRING);
                break;
            case "Z":
                this.getKind().setReturnType(OperationReturnType.BOOLEAN);
                break;
            case "I":
                this.getKind().setReturnType(OperationReturnType.NUMERIC_Z);
                break;
            default:
                assert(false); // shouldn't happen
                break;
        }
        this.sig = sig;
        init();
    }


    public void init() {
        if(this.kind.returnsNumericZ()) {
            this.automaton = new RegExp(Z_REXP).toAutomaton();
            this.range = NumRange.Z.clone();
        } else if (this.kind.returnsNumericN()) {
            this.automaton = new RegExp(N_REXP).toAutomaton();
            this.range = NumRange.N.clone();
        } else if (this.kind.returnsNumericLN()) {
            this.automaton = new RegExp(Z_REXP).toAutomaton();
            this.range = NumRange.Z.clone();
        } else if (this.kind.returnsString()) {
            this.automaton = new RegExp(STR_REXP).toAutomaton();
            this.range = NumRange.N.clone();
        } else if (this.kind.returnsBoolean()) {
            this.automaton = new RegExp(BOOL_REXP).toAutomaton();
            this.range = new BooleanRange();
        } else if (this.kind.returnsStringLower()) {
            this.automaton = new RegExp(STR_REXP_LOWER).toAutomaton();
            this.range = NumRange.N.clone();
        } else if (this.kind.returnsStringUpper()) {
            this.automaton = new RegExp(STR_REXP_UPPER).toAutomaton();
            this.range = NumRange.N.clone();
        }  else if (this.kind.returnsStringUpper()) {
            this.automaton = new RegExp(STR_REXP_TRIMMED).toAutomaton();
            this.range = NumRange.N.clone();
        }
    }


    @Override
    public int getKindId() {
        return this.kind.getId();
    }

    @Override
    public boolean isLiteral() {
        return AutomatonUtils.isLiteral(this.getAutomaton());
    }

    @Override
    public boolean isRegex() {
        return false;
    }

    @Override
    public boolean isNumeric() {
        return this.kind.returnsNumericZ() || this.kind.returnsNumericN() || this.kind.returnsNumericLN();
    }
    @Override
    public boolean isBoolean() {
        return this.kind.returnsBoolean();
    }

    @Override
    public boolean isVariable() {
        return false;
    }

    @Override
    public boolean isConstraint() {
        return false;
    }

    @Override
    public boolean isString() {
        return this.kind.returnsString();
    }

    public OperationKind getKind() {
        return kind;
    }

    @Override
    public String toString(){

        String ssig = "";

        if(this.sig != null)
            ssig = "\n" + this.sig.getMethodName();

        return "{" + this.id + "}" + "\n" + this.getRange().toString(); }

    @Override
    public String getLabel() {
        return this.label;
    }

    @Override
    public void setLabel(String label) {
        this.label = label;
    }

    @Override
    public void setKind(NetworkEntityKind kind) {
        assert(kind instanceof OperationKind);
        this.kind = (OperationKind)kind;
        this.sig = StandardWrappers.getSigForOperation(this.kind);
    }

    public JavaMethodSignature getSig() {
        return this.sig;
    }

    @Override
    public Operation clone() { return new Operation(this); };


}

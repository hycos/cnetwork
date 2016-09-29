package org.snt.cnetwork.core;

import dk.brics.automaton.Automaton;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.range.BasicRange;

public abstract class Node implements NetworkEntity, Cloneable {

    final static Logger logger = LoggerFactory.getLogger(Node.class);


    // most generic regular expressions
    public static String Z_REXP = "-?([0-9]|[1-9][0-9]{0,7})";
    public static String N_REXP = "[0-9]|([1-9][0-9]{0,7})";
    public static String STR_REXP = ".*";
    public static String STR_REXP_LOWER = "[^A-Z]*";
    public static String STR_REXP_UPPER = "[^a-z]*";
    public static String STR_REXP_TRIMMED = "[^ ]*.*[^ ]*";

    public static String BOOL_REXP = "0|1";

    protected int id;
    protected int kind;

    // each node is assigned one automaton
    protected Automaton automaton;
    protected BasicRange range;
    protected String instance;
    protected String annotation = "";

    protected boolean constraint = false;

    private static int nid = 0;

    public Node() {
        this.id = nid++;
        this.constraint = false;
        // start with the most general rule
    }

    public Node(Node other) {
        this.id = other.id;
        this.range = other.range.clone();
        this.automaton = other.getAutomaton().clone();
        this.constraint = other.constraint;
        this.annotation = other.annotation;
    }

    public int getId() {
        return this.id;
    }

    // Eveery single node can be uniqely identified
    @Override
    public int hashCode() {
        return this.id;
    }

    @Override
    public boolean equals(Object o) {

        if(!(o instanceof Node))
            return false;

        Node n = (Node)o;

        return this.id == n.id;
    }

    public Automaton getAutomaton() {
        return this.automaton;
    }

    public abstract int getKindId();

    public abstract NetworkEntityKind getKind();

    public void setAutomaton(Automaton a) {
        //LOGGER.info("set " + this.getId() + " " + a.getShortestExample(true));
        this.automaton = a.clone();
    }

    public boolean isOperation() {
        //LOGGER.info("ID " + getKindId());
        return getKindId()%2 == 0;
    }

    public boolean isOperand() {
        //LOGGER.info("ID " + getKindId());
        return getKindId()%2 != 0;
    }

    public abstract boolean isLiteral();

    public abstract boolean isRegex();

    public abstract boolean isString();

    public abstract boolean isNumeric();

    public abstract boolean isBoolean();

    public abstract boolean isVariable();

    public abstract boolean isConstraint();


    //public abstract boolean isConstraint();

    public void setInstance(String instance) {
        this.instance = instance;
    }

    public void setAsConstraint(boolean constraint) {
        this.constraint = constraint;
    }

    public BasicRange getRange() {
        return this.range;
    }

    public void setRange(BasicRange r) {
        assert(r.getMin() <= r.getMax());
        this.range = r.clone();
    }


    @Override
    public abstract String toString();

    public abstract String getLabel();

    public abstract void setLabel(String label);

    public abstract void setKind(NetworkEntityKind kind);

    public void annotate(String annotation) {
        this.annotation = annotation;
    }

    public boolean isAnnotated() {
        return !this.annotation.isEmpty();
    }

    public String getAnnotation() {
        return this.annotation;
    }

    @Override
    public abstract Node clone();


}

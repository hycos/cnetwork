package org.snt.cnetwork.core;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.*;

public abstract class Node implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(Node.class);

    protected final int id;

    protected String instance = "";
    protected String label = "";
    protected String annotation = "";
    protected NodeDomain dom;

    private static int nid = 0;

    protected NodeKind kind;

    public Node(String label, NodeKind kind) {
        this.id = nid++;
        this.kind = kind;
        this.label = label;
        LOGGER.debug(".. " + label + " " + kind.toString());
        // compute the appropriate domain automatically
        this.dom = NodeDomainFactory.INSTANCE.getDomain(this);
    }

    public Node(Node other) {
        this.id = other.id;
        this.dom = other.dom.clone();
        this.annotation = other.annotation;
        this.kind = other.getKind();
        this.label = other.getLabel();
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

        if (!(o instanceof Node))
            return false;

        Node n = (Node) o;

        return this.id == n.id;
    }

    public NodeDomain getDomain() {
        return this.dom;
    }

    public void setDomain(NodeDomain d) {
        this.dom = d;
    }

    public NodeKind getKind() {
        return this.kind;
    }

    public void setKind(NodeKind kind) {
        this.kind = kind;
    }

    public abstract boolean isOperation();

    public abstract boolean isOperand();

    public abstract boolean isLiteral();

    public abstract boolean isRegex();

    public abstract boolean isString();

    public abstract boolean isNumeric();

    public abstract boolean isBoolean();

    public abstract boolean isVariable();

    public abstract boolean isConstraint();

    public void setInstance(String instance) {
        this.instance = instance;
    }

    public String getDotLabel() {
        final StringBuilder s = new StringBuilder();
        s.append("{" + this.id + "}\\n");
        s.append(isAnnotated() ? getAnnotation() + "\\n" : "");
        s.append("dom:" + dom.toString()+"\\n");
        s.append("kind:" + getKind().getDesc() +"\\n");
        return s.toString();
    }

    public String getLabel() {
       return this.label;
    }

    @Override
    public String toString() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public void annotate(String annotation) {
        this.annotation = annotation;
    }

    public boolean isAnnotated() {
        return !this.annotation.isEmpty();
    }

    public String getAnnotation() {
        return this.annotation;
    }

    //@TODO:Julian just for convenience -- have to refactor this
    public Range getRange() {
        DomainInterface iface = this.dom.getDomain("range");
        assert iface instanceof Range;
        return (Range)iface;
    }

    //@TODO:Julian just for convenience -- have to refactor this
    public Automaton getAutomaton() {
        DomainInterface iface = this.dom.getDomain("automaton");
        assert iface instanceof Automaton;
        return (Automaton)iface;
    }

    //@TODO:Julian just for convenience -- have to refactor this
    public void setRange(Range r) {
        this.dom.setDomain(r);
    }

    //@TODO:Julian just for convenience -- have to refactor this
    public void setAutomaton(Automaton a) {
        this.dom.setDomain(a);
    }


    @Override
    public abstract Node clone();

}

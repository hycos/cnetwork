package org.snt.cnetwork.core;

import org.snt.cnetwork.utils.DomainUtils;

public class NodeDomain implements DomainInterface<NodeDomain> {


    private Automaton automaton;
    private Range range;


    public NodeDomain(NodeDomain other) {
        this.automaton = other.automaton.clone();
        this.range = other.range.clone();
    }

    public NodeDomain(Automaton a, Range r) {
        this.automaton = a;
        this.range = r;
    }

    public NodeDomain(Automaton a) {
        this(a, new NumRange(DomainUtils.getApproxLenRange(a)));
    }

    public NodeDomain(Range r) {
        this(DomainUtils.getAutomatonForRange(r),r);
    }

    @Override
    public NodeDomain intersect(NodeDomain y) {
        return new NodeDomain(automaton.intersect(y.automaton), range.intersect(y
                .range));
    }

    @Override
    public NodeDomain union(NodeDomain y) {
        return new NodeDomain(automaton.union(y.automaton), range.union(y.range));
    }

    @Override
    public NodeDomain minus(NodeDomain y) {
        return new NodeDomain(automaton.minus(y.automaton), range.minus(y.range));
    }

    @Override
    public NodeDomain complement(NodeDomain y) {
        return new NodeDomain(automaton.minus(y.automaton), range.complement(y
                .range));
    }

    @Override
    protected NodeDomain clone() {
        return new NodeDomain(this.automaton.clone(), this.range.clone());
    }

    @Override
    public boolean subsumes(NodeDomain y) {
        return this.automaton.subsumes(y.automaton) && this.range.subsumes(y.range);
    }

    @Override
    public boolean isSingleton() {
        return this.automaton.isSingleton() && this.range.isSingleton();
    }

    @Override
    public boolean isEmpty() {
        return this.automaton == null || this.automaton.isEmpty() || this
                .range.isEmpty();
    }

    public Automaton getAutomaton() {
        return automaton;
    }

    public void setAutomaton(Automaton automaton) {
        this.automaton = automaton;
    }

    public Range getRange() {
        return range;
    }

    public void setRange(Range range) {
        this.range = range;
    }

}

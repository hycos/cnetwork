package org.snt.cnetwork.core;

import org.snt.cnetwork.utils.DomainUtils;


/**
 * This class is used in order to dispatch to dk.brics (we would like to stay
 * independent w.r.t. the underlying automaton library.
 */
public class Automaton extends dk.brics.automaton.Automaton implements
        DomainInterface<Automaton> {

    public Automaton() {
    }

    public Automaton(Automaton a) {
        this.union(a);
    }

    @Override
    public Automaton intersection(dk.brics.automaton.Automaton a) {
        throw new UnsupportedOperationException("pleas use intersect instead");
    }

    @Override
    public Automaton intersect(Automaton y) {
        super.intersection(y);
        return this;
    }

    @Override
    public Automaton union(Automaton y) {
        super.union(y);
        return this;
    }

    @Override
    public Automaton minus(Automaton y) {
        super.minus(y);
        return this;
    }

    @Override
    public Automaton complement(Automaton y) {
        super.minus(y);
        return this;
    }

    @Override
    public boolean subsumes(Automaton y) {
        return super.intersection(y).equals(y);
    }

    @Override
    public boolean isSingleton() {
        return DomainUtils.isLiteral(this);
    }

    public Automaton concatenate(Automaton y) {
        super.concatenate(y);
        return this;
    }


}

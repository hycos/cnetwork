package org.snt.cnetwork.core;

import org.snt.cnetwork.utils.DomainUtils;


/**
 * This class is used in order to dispatch to dk.brics (we would like to stay
 * independent w.r.t. the underlying automaton library.
 */
public class Automaton extends dk.brics.automaton.Automaton implements
        DomainInterface<Automaton> {


    private Automaton wrap(dk.brics.automaton.Automaton o) {
        Automaton a = new Automaton();
        a.union(this);
        return a;
    }

    @Override
    public Automaton intersection(dk.brics.automaton.Automaton a) {
        throw new UnsupportedOperationException("pleas use intersect instead");
    }

    @Override
    public Automaton intersect(Automaton y) {
        dk.brics.automaton.Automaton a = super.intersection(y);
        return wrap(a);
    }

    @Override
    public Automaton union(Automaton y) {
        dk.brics.automaton.Automaton a = super.union(y);
        return wrap(a);
    }

    @Override
    public Automaton minus(Automaton y) {
        dk.brics.automaton.Automaton a = super.minus(y);
        return wrap(a);
    }

    @Override
    public Automaton complement(Automaton y) {
        dk.brics.automaton.Automaton a = super.minus(y);
        return wrap(a);
    }

    @Override
    public boolean subsumes(Automaton y) {
        return super.intersection(y).equals(y);
    }

    @Override
    public boolean isSingleton() {
        return DomainUtils.isLiteral(this);
    }

    @Override
    public Automaton clone(){
        return wrap(this);
    }


    public Automaton concatenate(Automaton y) {
        dk.brics.automaton.Automaton a = super.concatenate(y);
        return wrap(a);
    }

}

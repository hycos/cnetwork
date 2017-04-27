package org.snt.cnetwork.core.domain.automaton;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class Multimat implements Automaton<Multimat> {

    List<SimpleAutomaton> acomponents = new Vector();
    Map<Integer, Set<AutomatonCut>> acuts = new HashMap<>();
    SimpleAutomaton combined = new SimpleAutomaton();


    final static Logger LOGGER = LoggerFactory.getLogger(Multimat.class);

    public Multimat(SimpleAutomaton simple) {
        combined = simple;
        acomponents.add(combined);
    }

    public Multimat(String rexp) {
        combined = new SimpleAutomaton(rexp);
        acomponents.add(combined);
    }

    public Multimat() {

    }

    @Override
    public Automaton concatenate(Multimat y) {
        Multimat n = new Multimat();

        n.acomponents.addAll(this.acomponents);
        n.acomponents.addAll(y.acomponents);

        n.acuts.putAll(this.acuts);
        n.acuts.putAll(y.acuts);

        n.combined = combined.concatenate(y.combined);
        return n;
    }

    @Override
    public Automaton intersect(Multimat y) {

        Multimat first = this;
        Multimat second = y;


        //Map<Integer, Set<AutomatonCut>> acuts = new HashMap<>();

        return null;
    }

    @Override
    public Automaton union(Multimat y) {
        return new Multimat(this.combined.union(y.combined));
    }

    @Override
    public Automaton minus(Multimat y) {
        return new Multimat(this.combined.minus(y.combined));
    }

    @Override
    public Automaton complement() {
       return new Multimat(this.combined.complement());
    }
}

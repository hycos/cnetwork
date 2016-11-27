package org.snt.cnetwork.core;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class RegExp extends dk.brics.automaton.RegExp {

    final static Logger LOGGER = LoggerFactory.getLogger(RegExp.class);

    public RegExp(String s) throws IllegalArgumentException {
        super(s);
    }

    @Override
    public Automaton toAutomaton() {
        dk.brics.automaton.Automaton a = super.toAutomaton();
        Automaton na = new Automaton();
        ((dk.brics.automaton.Automaton)na).union(a);
        return na;
    }


}

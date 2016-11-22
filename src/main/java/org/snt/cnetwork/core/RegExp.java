package org.snt.cnetwork.core;

import dk.brics.automaton.*;

import java.io.IOException;


public class RegExp extends dk.brics.automaton.RegExp {


    private static class Provider implements AutomatonProvider {
        @Override
        public Automaton getAutomaton(String name) throws IOException {
            return new Automaton();
        }
    }

    private static final Provider provider = new Provider();

    public RegExp(String s) throws IllegalArgumentException {
        super(s);
    }

    public RegExp(String s, int syntax_flags) throws IllegalArgumentException {
        super(s, syntax_flags);
    }

    @Override
    public Automaton toAutomaton() {
        dk.brics.automaton.Automaton a = super.toAutomaton(provider);
        assert a instanceof Automaton;
        return (Automaton)a;
    }


}

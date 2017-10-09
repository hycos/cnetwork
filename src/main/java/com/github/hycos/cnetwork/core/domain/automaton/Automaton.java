package com.github.hycos.cnetwork.core.domain.automaton;


public interface Automaton<T extends Automaton> {
    Automaton concatenate(T y);
    Automaton intersect(T y);
    Automaton union(T y);
    Automaton minus(T y);
    Automaton complement();
}

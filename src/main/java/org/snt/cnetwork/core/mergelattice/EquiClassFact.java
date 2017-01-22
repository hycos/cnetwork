package org.snt.cnetwork.core.mergelattice;

public interface EquiClassFact<T> {
    EquiClass create(T [] ... pars);
    String computeLabel(String ... s);
}

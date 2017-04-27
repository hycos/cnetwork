package org.snt.cnetwork.core.domain.automaton;


public class AutomatonCut {

    private int first;
    private int second;

    public AutomatonCut(int first, int second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean equals(Object o){
        return ((AutomatonCut) o).first == first && ((AutomatonCut) o).second
                == second;
    }

}

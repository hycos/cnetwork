package org.snt.cnetwork.core.domain.automaton;

import dk.brics.automaton.State;
import dk.brics.automaton.Transition;
import org.snt.cnetwork.core.domain.DomainInterface;
import org.snt.cnetwork.core.domain.automaton.trackauto.TrackAutomaton;
import org.snt.cnetwork.core.domain.automaton.trackauto.TrackAutomatonNode;
import org.snt.cnetwork.core.domain.range.NumRange;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.utils.DomainUtils;

import java.util.HashSet;
import java.util.Set;


public class TrackingAutomaton implements Automaton<TrackingAutomaton>,
        DomainInterface<TrackingAutomaton> {

    private TrackAutomaton auto;

    public TrackingAutomaton(TrackAutomaton auto) {
        this.auto = new TrackAutomaton(auto);
    }

    public TrackingAutomaton(String name, String rexp) {
        this.auto = new TrackAutomaton(name, rexp);
    }


    public TrackingAutomaton(String rexp) {
        this.auto = new TrackAutomaton("", rexp);
    }

    public TrackingAutomaton(SimpleAutomaton auto) {
        this.auto = new TrackAutomaton(TrackAutomatonNode.Kind.LEAF, auto
                .getAutomaton(), DomainUtils.getApproxLenRange(new
                SimpleAutomaton(auto
                .getAutomaton())));
    }

    @Override
    public TrackingAutomaton concatenate(TrackingAutomaton y) {
        return new TrackingAutomaton(auto.concatenate(y.auto));
    }

    @Override
    public void init(Node n) {
        this.auto.setName("" + n.getId());
    }

    @Override
    public TrackingAutomaton intersect(TrackingAutomaton y) {
        return new TrackingAutomaton(auto.intersect(y.auto));
    }

    @Override
    public TrackingAutomaton union(TrackingAutomaton y) {
        return new TrackingAutomaton(auto.union(y.auto));
    }

    @Override
    public TrackingAutomaton minus(TrackingAutomaton y) {
        return new TrackingAutomaton(auto.minus(y.auto));
    }

    @Override
    public boolean subsumes(TrackingAutomaton y) {
        TrackAutomaton isect = auto.intersect(y.auto);
        return isect.equals(y.auto);
    }

    @Override
    public boolean isSingleton() {
        return isLiteral();
    }

    @Override
    public boolean isEmpty() {
        return auto.getRoot().getAutomaton() == null ||
                auto.getRoot().getAutomaton().isEmpty();
    }

    @Override
    public TrackingAutomaton complement() {
        return new TrackingAutomaton(auto.complement());
    }

    @Override
    public String getDomainName() {
        return "track-automaton";
    }

    @Override
    public TrackingAutomaton clone() {
        return new TrackingAutomaton(new TrackAutomaton(auto));
    }

    public void setRange(NumRange nr) {
        auto.getRoot().setRange(nr);
    }
    public void setName(String name) {
        auto.setName(name);
    }

    public String getName() {
        return auto.getName();
    }

    @Override
    public boolean equals(Object o) {

        if(!(o instanceof TrackingAutomaton))
            return false;

        TrackingAutomaton to = (TrackingAutomaton)o;

        return to.auto.getRoot().getAutomaton().equals(auto.getRoot()
                .getAutomaton());
    }

    public String toDot() {
        return auto.toDot();
    }

    @Override
    public String toString() {
        return "|t|";
    }



    private boolean isLiteral() {
        State init = auto.getRoot().getAutomaton().getInitialState();
        State ptr = init;
        Set<State> visited = new HashSet<>();
        while(ptr.getTransitions().size() == 1) {
            Transition trans = ptr.getTransitions().iterator().next();
            if(trans.getMax() != trans.getMin())
                return false;

            State next = ptr.getTransitions().iterator().next().getDest();

            if(visited.contains(next))
                return false;

            visited.add(next);

            // go to the next state
            ptr = next;
        }

        // final
        return ptr.getTransitions().size() == 0 && ptr.isAccept();
    }
}

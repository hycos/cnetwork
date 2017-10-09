/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetwork.core.domain.automaton;

import com.github.hycos.cnetwork.core.domain.DomainInterface;
import com.github.hycos.cnetwork.core.domain.automaton.trackauto.TrackAutomaton;
import com.github.hycos.cnetwork.core.domain.automaton.trackauto.TrackAutomatonNode;
import com.github.hycos.cnetwork.core.domain.range.NumRange;
import dk.brics.automaton.State;
import dk.brics.automaton.Transition;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.utils.DomainUtils;

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

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
import dk.brics.automaton.State;
import dk.brics.automaton.Transition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.autorex.Autorex;
import com.github.hycos.cnetwork.core.graph.Node;

import java.util.*;

/**
 * This class is used in order to dispatch to dk.brics (we would like to stay
 * independent w.r.t. the underlying automaton library.
 */
public class SimpleAutomaton implements Automaton<SimpleAutomaton>, DomainInterface<SimpleAutomaton> {

    final static Logger LOGGER = LoggerFactory.getLogger(SimpleAutomaton.class);

    private static SimpleAutomaton ALL_ACCEPT = new SimpleAutomaton(".*");

    private dk.brics.automaton.Automaton a = null;


    public SimpleAutomaton(String rexp) {
        this(new dk.brics.automaton.RegExp(rexp).toAutomaton());
    }

    public SimpleAutomaton() {
        this.a = new dk.brics.automaton.Automaton();
    }

    public SimpleAutomaton(dk.brics.automaton.Automaton a){
        this.a = a;
    }

    public String getSingleton() {
        return this.a.getSingleton();
    }


    public String toDot() {
        return this.a.toDot();
    }

    @Override
    public boolean equals(Object o) {

        if(!(o instanceof SimpleAutomaton))
            return false;

        return this.a.equals(((SimpleAutomaton)o).a);
    }

    public SimpleAutomaton concatenate(SimpleAutomaton y) {
        return new SimpleAutomaton(this.a.concatenate(y.a));
    }

    @Override
    public void init(Node n) {
        // nothing to do
    }

    @Override
    public SimpleAutomaton intersect(SimpleAutomaton y) {
        return new SimpleAutomaton(this.a.intersection(y.a));
    }

    @Override
    public SimpleAutomaton union(SimpleAutomaton y) {
        return new SimpleAutomaton(this.a.union(y.a));
    }

    @Override
    public SimpleAutomaton minus(SimpleAutomaton y) {
        return new SimpleAutomaton(this.a.minus(y.a));
    }

    @Override
    public SimpleAutomaton complement() {
        return new SimpleAutomaton(a.complement());
    }

    @Override
    public boolean subsumes(SimpleAutomaton y) {
        return a.intersection(y.a).equals(y.a);
    }

    @Override
    public boolean isSingleton() {
        return isLiteral();
    }

    @Override
    public boolean isEmpty() {
        return a.isEmpty() || a == null;
    }

    @Override
    public String getDomainName() {
        return "automaton";
    }

    @Override
    public SimpleAutomaton clone() {
        return new SimpleAutomaton(this.a.clone());
    }

    public Set<String> getFiniteStrings(){
        return a.getFiniteStrings();
    }

    public boolean isFinite() {
        return a.isFinite();
    }

    public boolean isEmptyString() {
        return this.a.isEmptyString();
    }

    public String getShortestExample() {
        return this.a.getShortestExample(true);
    }

    private boolean isLiteral() {
        State init = a.getInitialState();
        State ptr = init;
        Set<State> visited = new HashSet<State>();
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

    public SimpleAutomaton getAllAcceptingSubstringsAutomaton() {
        return new SimpleAutomaton(Autorex.getSubstringAutomaton(this.a));
    }

    public SimpleAutomaton getAllAcceptingSuffixAutomaton() {
        return new SimpleAutomaton(Autorex.getSuffixAutomaton(this.a));
    }

    public SimpleAutomaton getSubstringAutomaton() {
        SimpleAutomaton pfx = ALL_ACCEPT.clone();
        SimpleAutomaton sfx = ALL_ACCEPT.clone();

        SimpleAutomaton result = pfx.concatenate(this).concatenate(sfx);
        result.a.minimize();
        return result;
    }

    public SimpleAutomaton getPfxAutomaton() {
        SimpleAutomaton pfx = ALL_ACCEPT.clone();
        SimpleAutomaton result = this.clone().concatenate(pfx);
        result.a.minimize();
        return result;
    }

    public SimpleAutomaton getSfxAutomaton() {
        SimpleAutomaton sfx = ALL_ACCEPT.clone();
        SimpleAutomaton result = sfx.concatenate(this.clone());
        result.a.minimize();
        return result;
    }

    public SimpleAutomaton getWrappedInSpacesAutomaton() {
        SimpleAutomaton pfx = ALL_ACCEPT.clone();
        SimpleAutomaton sfx = ALL_ACCEPT.clone();
        SimpleAutomaton result = pfx.concatenate(this).concatenate(sfx);
        result.a.minimize();
        return result;
    }

    public SimpleAutomaton getCamelCaseAutomaton() {
        return new SimpleAutomaton(Autorex.getCamelCaseAutomaton(this.a));
    }

    public int getLongestExample() {

        LinkedList<LinkedList<Transition>> paths = new LinkedList<LinkedList<Transition>>();

        for (Transition t : a.getInitialState().getTransitions()) {
            LinkedList<Transition> path = new LinkedList<Transition>();
            path.add(t);
            paths.add(path);
        }

        int max = 0;

        while (!paths.isEmpty()) {

            LinkedList<Transition> item = paths.pollFirst();

            if (item.size() > max) {
                max = item.size();
            }

            State last = item.getLast().getDest();

            Set<Transition> toConsider = new HashSet<Transition>();
            toConsider.addAll(last.getTransitions());
            toConsider.removeAll(item);

            int lidx = toConsider.size() - 1;

            for (Transition t : toConsider) {
                if (lidx == 0) {
                    item.addLast(t);
                    paths.addLast(item);
                } else {
                    LinkedList<Transition> cp = new LinkedList<Transition>();
                    cp.addAll(item);
                    cp.addLast(t);
                    paths.addLast(cp);
                }
                lidx--;
            }


        }
        return max;
    }


    /**
     *
     * Generates the string based on regex that is passed as
     * a parameter to the constructor.
     *
     * @return the generated string.
     *
     */
    public String getRandomString(int minsize) {
        StringBuilder builder = new StringBuilder();
        generate(builder, a.getInitialState(), minsize);
        return builder.toString();
    }


    /**
     *
     * Recursive helper function that interacts with the dk.brics libraries
     * to generate the string based on the regular expression.
     *
     * @param sb a string builder.
     * @param s a state of dk.brics's state machine.
     */
    private static void generate(StringBuilder sb, State s, int minsize) {

        Random rgen = new Random();

        List<Transition> transitions = s.getSortedTransitions(true);

        if (transitions.size() == 0) {
            return;
        }

        int choices = s.isAccept() ? transitions.size() : transitions.size() - 1;
        int choice = getRandomInt(0, choices, rgen);


        if (s.isAccept() && choice == 0 && sb.length() >= minsize)
            return;

        LOGGER.info(transitions.size() + " --- " + choice);

        Transition t = transitions.get(choice);

        sb.append((char) getRandomInt(t.getMin(), t.getMax(), rgen));

        generate(sb, t.getDest(), minsize);
    }

    @Override
    public String toString() {
        if(this.isLiteral()){
            return getShortestExample();
        } else {
            return "|a|";
        }
    }

    /**
     *
     * A helper that returns a random integer within a given range.
     *
     * @param min lower bound.
     * @param max upper bound.
     * @param random random generator.
     * @return a randomly generated int.
     */
    public final static int getRandomInt(int min, int max, Random random) {
        return (min + Math.round(random.nextFloat() * (max - min)));
    }

    public dk.brics.automaton.Automaton getAutomaton() {
        return this.a.clone();
    }

    public boolean isTotal() {
        return this.a.isTotal();
    }

    public String getRegex() {
        return Autorex.getRegexFromAutomaton(this.a);
    }

    public boolean run(String s){
        return this.a.run(s);
    }

    public Set<String> getStrings(int number){
        return this.a.getStrings(number);
    }
}

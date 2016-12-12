package org.snt.cnetwork.core.domain;


import dk.brics.automaton.State;
import dk.brics.automaton.Transition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.autorex.Autorex;

import java.util.*;

/**
 * This class is used in order to dispatch to dk.brics (we would like to stay
 * independent w.r.t. the underlying automaton library.
 */
public class Automaton implements DomainInterface<Automaton> {

    final static Logger LOGGER = LoggerFactory.getLogger(Automaton.class);

    private static Automaton ALL_ACCEPT = new Automaton(".*");

    private dk.brics.automaton.Automaton a = null;

    public Automaton(String rexp) {
        this(new dk.brics.automaton.RegExp(rexp).toAutomaton());
    }

    public Automaton() {
        this.a = new dk.brics.automaton.Automaton();
    }

    public Automaton(dk.brics.automaton.Automaton a){
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

        if(!(o instanceof  Automaton))
            return false;

        return this.a.equals(((Automaton)o).a);
    }

    public Automaton concatenate(Automaton y) {
        return new Automaton(this.a.concatenate(y.a));
    }

    @Override
    public Automaton intersect(Automaton y) {
        return new Automaton(this.a.intersection(y.a));
    }

    @Override
    public Automaton union(Automaton y) {
        return new Automaton(this.a.union(y.a));
    }

    @Override
    public Automaton minus(Automaton y) {
        return new Automaton(this.a.minus(y.a));
    }

    @Override
    public Automaton complement() {
        return new Automaton(a.complement());
    }

    @Override
    public boolean subsumes(Automaton y) {
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
    public Automaton clone() {
        return new Automaton(this.a.clone());
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
        return ptr.isAccept();
    }

    public Automaton getAllAcceptingSubstringsAutomaton() {
        return new Automaton(Autorex.getSubstringAutomaton(this.a));
    }

    public Automaton getAllAcceptingSuffixAutomaton() {
        return new Automaton(Autorex.getSuffixAutomaton(this.a));
    }

    public Automaton getSubstringAutomaton() {
        Automaton pfx = ALL_ACCEPT.clone();
        Automaton sfx = ALL_ACCEPT.clone();

        Automaton result = pfx.concatenate(this).concatenate(sfx);
        result.a.minimize();
        return result;
    }

    public Automaton getPfxAutomaton() {
        Automaton pfx = ALL_ACCEPT.clone();
        Automaton result = this.clone().concatenate(pfx);
        result.a.minimize();
        return result;
    }

    public Automaton getSfxAutomaton() {
        Automaton sfx = ALL_ACCEPT.clone();
        Automaton result = sfx.concatenate(this.clone());
        result.a.minimize();
        return result;
    }

    public Automaton getWrappedInSpacesAutomaton() {
        Automaton pfx = ALL_ACCEPT.clone();
        Automaton sfx = ALL_ACCEPT.clone();
        Automaton result = pfx.concatenate(this).concatenate(sfx);
        result.a.minimize();
        return result;
    }

    public Automaton getCamelCaseAutomaton() {
        return new Automaton(Autorex.getCamelCaseAutomaton(this.a));
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
        return this.a;
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

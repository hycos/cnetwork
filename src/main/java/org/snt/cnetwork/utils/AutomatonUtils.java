package org.snt.cnetwork.utils;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;
import dk.brics.automaton.State;
import dk.brics.automaton.Transition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.range.BasicRange;
import org.snt.cnetwork.core.range.BooleanRange;
import org.snt.cnetwork.core.range.NumRange;

import java.util.*;

public class AutomatonUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(AutomatonUtils.class);

    private static int transId = 0;


    private static Character[] sarray = new Character[]{'+', '{', '}', '(', ')', '[', ']', '&', '^', '-', '?', '*', '\"', '$', '<', '>', '.', '|', '@', '#'};
    private static Set<Character> special = new HashSet<Character>(Arrays.asList(sarray));


    public static Automaton getStrAutomatonForRange(long min, long max) {
        return new RegExp(".{" + Math.max(0,min) + "," + max + "}").toAutomaton();
    }

    public static Automaton getStrAutomatonForRange(BasicRange r) {
        return getStrAutomatonForRange(Math.max(0,r.getMin()), r.getMax());
    }

    public static Automaton getNumAutomatonForRange(BasicRange r) {
        return getNumAutomatonForRange(r.getMin(), r.getMax());
    }

    public static Automaton getBoolAutomatonForRange(BooleanRange r) {

        String strue = BooleanRange.BooleanValue.TRUE.getValue();
        String sfalse = BooleanRange.BooleanValue.FALSE.getValue();

        if(r.isAlwaysTrue()) {
            return new RegExp(strue).toAutomaton();
        } else if (r.isAlwaysFalse()) {
            return new RegExp(sfalse).toAutomaton();
        }

        return new RegExp(strue + "|" + sfalse).toAutomaton();
    }


    public static Automaton getNumAutomatonForRange(long min, long max) {

        assert (max >= min);

        if (max == min) {
            return new RegExp(String.valueOf(min)).toAutomaton();
        }

        String mins = RexpUtils.getRexpForMin(min);
        //logger.info("MINS " + mins);
        String maxs = RexpUtils.getRexpForMax(max);
        //logger.info("MAXS " + maxs);

        RegExp minr = new RegExp(mins);
        RegExp maxr = new RegExp(maxs);


        Automaton mina = minr.toAutomaton();
        Automaton maxa = maxr.toAutomaton();


        return mina.intersection(maxa);
    }

    public static BasicRange getApproxLenRange(Automaton a) {
        assert (a != null);

        int minlen = 0;

        if (a.isEmpty() || a.isEmptyString()) {
            minlen = 0;
        } else {
            minlen = a.getShortestExample(true).length();
        }

        if (!a.isFinite()) {
        //    logger.info("SHORTEST " + a.getShortestExample(true));
            return new BasicRange(minlen, Integer.MAX_VALUE);
        }

        BasicRange nr = new BasicRange(minlen, getLongestExample(a));


        return nr;
    }

    public static BasicRange getExactLenRange(Automaton a) {
        assert (a != null);

        Set<String> result = a.getFiniteStrings();

        NumRange nr = new NumRange();

        for(String s : result) {
            nr.add(s.length());
        }

        return nr;
    }

    public static BasicRange getNumRangeForAutomaton(Automaton a) {

        String r = RexpUtils.getRexpForRange(NumRange.Z.getMin(), NumRange.Z.getMax());

        Automaton ra = new RegExp(r).toAutomaton();

        if(!ra.intersection(a).equals(a))
            return null;

        LOGGER.info(a.toString());


        Set<String> result = a.getFiniteStrings();

        NumRange nr = new NumRange();

        for(String s : result) {
            nr.add(Integer.parseInt(s));
        }

        return nr;
    }



    public static int getLongestExample(Automaton a) {

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


    public static String escapeSpecialCharacters(String s) {
        StringBuilder out = new StringBuilder();
        char pred = ' ';
        for (char c : s.toCharArray()) {
            if (pred != '\\' && special.contains(c)) {
                out.append("\\" + c);
            } else if (pred == '\\' && special.contains(c)) {
                out.deleteCharAt(out.length() - 1); // delete NULL
                out.append(c);
            } else {
                out.append(c);
            }
            pred = c;
        }
        return out.toString();
    }

    public static Automaton getSubstringAutomaton(Automaton a) {
        Automaton pfx = new RegExp(".*").toAutomaton();
        Automaton sfx = new RegExp(".*").toAutomaton();

        Automaton result = pfx.concatenate(a).concatenate(sfx);
        result.minimize();

        return result;
    }

    public static Automaton getPfxAutomaton(Automaton a) {
        Automaton pfx = new RegExp(".*").toAutomaton();

        Automaton result = a.concatenate(pfx);
        result.minimize();

        return result;
    }

    public static Automaton getSfxAutomaton(Automaton a) {
        Automaton sfx = new RegExp(".*").toAutomaton();

        Automaton result = sfx.concatenate(a);
        result.minimize();

        return result;
    }

    public static Automaton getWrappedInSpacesAutomaton(Automaton a) {
        Automaton pfx = new RegExp(" *").toAutomaton();
        Automaton sfx = new RegExp(" *").toAutomaton();

        Automaton result = pfx.concatenate(a).concatenate(sfx);
        result.minimize();

        return result;
    }



    /**
     *
     * Generates the string based on regex that is passed as
     * a parameter to the constructor.
     *
     * @return the generated string.
     *
     */
    public static String getRandomString(Automaton a, int minsize) {
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

    public static boolean isLiteral(Automaton a) {

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
    
}

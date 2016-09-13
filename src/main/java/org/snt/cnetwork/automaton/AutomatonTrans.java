package org.snt.cnetwork.automaton;

import dk.brics.automaton.*;
import org.apache.commons.lang3.StringEscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.utils.AutomatonUtils;
import org.snt.cnetwork.utils.CharUtils;

import java.util.*;

public class AutomatonTrans extends Automaton {

    final static Logger logger = LoggerFactory.getLogger(AutomatonTrans.class);

    public static enum Kind {

        SUFFIX(1,"suffix"),
        NORMAL(3,"normal"),
        CAMEL(5, "camel"),
        SUBSTRING(7, "substring");

        private final String sval;
        private final int ival;

        Kind(int ival, String sval) {
            this.sval = sval;
            this.ival = ival;
        }

        public int getId() {
            return this.ival;
        }

        public String toString() {
            return this.sval;
        }

        public static Kind KindFromString(String kind) {
            switch(kind) {
                case "suffix": return SUFFIX;
                case "normal": return NORMAL;
                case "camel" : return CAMEL;
                case "substring": return SUBSTRING;

            }
            // should never ever happen
            assert(false);
            return null;
        }

        public boolean isSuffix() {
            return this == SUFFIX;
        }
        public boolean isNormal() {
            return this == NORMAL;
        }
        public boolean isCamel() {
            return this == CAMEL;
        }
        public boolean isSubstring() {return this == SUBSTRING;}


    }

    private Kind kind;
    private Map<State, Integer> statenumber = null;
    private Map<Integer, State> numberstate = null;
    private Map<Integer, HashSet<Integer>> adjacency = null;

    HashMap<State, HashSet<FullTransition>> incoming = null;
    HashMap<State, HashSet<FullTransition>> outgoing = null;
    HashSet<FullTransition> transitions = null;

    private int stateId;

    private static Automaton any = new RegExp(".*").toAutomaton();

    public AutomatonTrans() {
        super();
        this.statenumber = new HashMap<State,Integer>();
        this.numberstate = new HashMap<Integer,State>();
        this.adjacency = new HashMap<Integer, HashSet<Integer>>();
        this.stateId = 0;
        this.incoming = new HashMap<>();
        this.outgoing = new HashMap<>();
        this.kind = Kind.NORMAL;
        this.transitions = new HashSet<FullTransition>();
    }

    public AutomatonTrans(Automaton a) {
        this();
        this.setInitialState(a.getInitialState());
        finalize();
        prepare();
    }

    public AutomatonTrans(String rexp) {
        this(new RegExp(rexp).toAutomaton());
    }

    private void set() {
        for (State s : this.getStates()) {
            s.setAccept(true);
        }
    }

    private void setAccepting() {
        for (State s : this.getStates()) {
            s.setAccept(true);
        }
    }


    private void prepare() {

        // init state has no incomings

        // get all transitions
        for (State s : this.getStates()) {
            for (Transition t : s.getTransitions()) {
                    FullTransition ft = new FullTransition(s, t, t.getDest());
                    addToIncoming(ft);
                    addToOutgoing(ft);
                    this.transitions.add(ft);
                    addToAdjacency(getNumberOfState(ft.getSourceState()), getNumberOfState(ft.getTargetState()));
                }
            }
        }


    private void addToAdjacency(int src, int dest) {
       if(!this.adjacency.containsKey(src)) {
           this.adjacency.put(src,new HashSet<Integer>());
       }
        this.adjacency.get(src).add(dest);
    }

    private void addToIncoming(FullTransition ft) {
        if (!incoming.containsKey(ft.getTargetState())) {
            incoming.put(ft.getTargetState(), new HashSet<FullTransition>());
        }
        incoming.get(ft.getTargetState()).add(ft);
    }

    private void addToOutgoing(FullTransition ft) {
        if (!outgoing.containsKey(ft.getSourceState())) {
            outgoing.put(ft.getSourceState(), new HashSet<FullTransition>());
        }
        outgoing.get(ft.getSourceState()).add(ft);
    }


    private Set<State> findMatchingPredecessor(State s, char c) {

        Set<State> result = new HashSet<State>();

        if(!this.incoming.containsKey(s))
            return null;

        for(FullTransition t : this.incoming.get(s)) {

            Transition tt = t.getLastTran();
            if(tt.getMin() <= c && tt.getMax() >= c) {
                //logger.info("matching predecessor " + getNumberOfState(t.getSourceState()));
                result.add(t.getSourceState());
            }

        }
        return result;
    }

    public Set<StatePair> getStateBoundaries(String s) {

        assert(this.kind.isNormal());
        AutomatonTrans allaccept = new AutomatonTrans(this.clone());
        allaccept.convertToSubstringAutomaton();
        //assert(allaccept.kind.isSuffix());

        List<Integer> list = new Vector<Integer>();
        List<State> init = new Vector<State>();

        init.add(allaccept.getInitialState());

        LinkedList<List<State>> worklist1 = new LinkedList<>();
        LinkedList<List<State>> worklist2 = new LinkedList<>();
        worklist1.add(init);


        int sptr = 0;

        while(sptr < s.length()) {
            while(!worklist1.isEmpty()){

                List<State> elem = worklist1.poll();

                State last = elem.get(elem.size() - 1);
                for(Transition t : last.getTransitions()) {
                    int sidx = allaccept.getNumberOfState(last);
                    int eidx = allaccept.getNumberOfState(t.getDest());

                    //logger.info("from " + sidx + " to " + eidx);


                    if(s.charAt(sptr) >= t.getMin() && s.charAt(sptr) <= t.getMax()) {
                        // match - only consider 'original' transitions, no epsilons

                        if(last == allaccept.getInitialState()) {
                            Set<State> result = allaccept.findMatchingPredecessor(t.getDest(),s.charAt(sptr));
                            if(result.size() > 1)
                                result.remove(last); // remove initial state

                            for(State ns : result) {
                                List<State> nelem = new Vector<State>();
                                nelem.add(ns);
                                nelem.add(t.getDest());
                                worklist2.add(nelem);
                            }
                        } else {
                            List<State> nelem = new Vector<State>();
                            nelem.addAll(elem);
                            nelem.add(t.getDest());
                            //logger.info("addAll " + allaccept.getNumberOfState(t.getDest()));
                            worklist2.add(nelem);
                        }
                    } else {
                        continue;
                    }
                }
            }

            worklist1.addAll(worklist2);
            worklist2.clear();
            sptr ++;
        }

        Set<StatePair> spairs = new HashSet<StatePair>();


        //logger.info(this.toDot());
        for(List<State> elem : worklist1) {
            assert(elem.size() >= 1);

            // first map the states from the suffix automaton to the real one

            int sidx = allaccept.getNumberOfState(elem.get(0));
            int eidx = allaccept.getNumberOfState(elem.get(elem.size()-1));

            State sids = this.numberstate.get(sidx);
            State eids = this.numberstate.get(eidx);

            assert(sids != null);
            assert(eids != null);
            spairs.add(new StatePair(sids, eids));
        }


        return spairs;
    }


    private int searchAcceptNodeFrom(State start) {

        if(start.isAccept()) {
            //logger.info(getNumberOfState(start) + " is accept");
            return 0;
        }

        List<State> state = new Vector<>();
        state.add(start);
        LinkedList <List<State>> worklist = new LinkedList<>();
        worklist.add(state);

        int mindiff = -1;

        while(!worklist.isEmpty()) {
            List<State> s = worklist.pop();
            State last = s.get(s.size() - 1);

            if(s.size() > mindiff && mindiff >= 0)
                continue;

            if(!this.outgoing.containsKey(last)) {
                logger.info("no outgoings for " + getNumberOfState(last));
                continue;
            }

            for(FullTransition t : this.outgoing.get(last)) {

                //ogger.info("trnans " + t.toString());
                if(s.contains(t.getTargetState())) {
                    continue; // never visit a state twice
                }

                if(t.getTargetState().isAccept()) {
                    if(s.size() < mindiff || mindiff < 0) {
                        mindiff = s.size();
                    }
                    continue;
                } else {
                    List<State> scp = new Vector<>();
                    scp.addAll(s);
                    scp.add(t.getTargetState());
                    worklist.add(scp);
                }
            }
        }
        return mindiff;
    }

    private int searchfromEndToStart(State end) {

        if(end.equals(this.getInitialState()))
            return 0;

        List<State> state = new Vector<>();
        state.add(end);
        LinkedList <List<State>> worklist = new LinkedList<>();
        worklist.add(state);

        int mindiff = -1;

        while(!worklist.isEmpty()) {
            List<State> s = worklist.pop();

            State last = s.get(s.size()-1);

            if(s.size() > mindiff && mindiff >= 0)
                continue;

            for(FullTransition t : incoming.get(last)) {

                //logger.info(t.toString());
                if(s.contains(t.getSourceState())) {
                    continue; // never visit a state twice
                }

                if(t.getSourceState().equals(this.getInitialState())) {
                    if(s.size() < mindiff || mindiff < 0) {
                        mindiff = s.size();
                    }
                    continue;
                } else {
                    List<State> scp = new Vector<>();
                    scp.addAll(s);
                    scp.add(t.getSourceState());
                    worklist.add(scp);
                }
            }
        }
        return mindiff;
    }

    public int longestMatch ( String text ){
        Automaton a = new RegExp(StringEscapeUtils.escapeJava(text)).toAutomaton();

        AutomatonTrans testa = new AutomatonTrans(a.clone());

        testa.convertToSubstringAutomaton();

        //logger.info(testa.toDot());

        AutomatonTrans testb = this.clone();
        testb.convertToSubstringAutomaton();
        Automaton isect = testa.intersection(testb);

        if(isect == null || isect.isEmpty() || isect.isEmptyString()) {
            return 0;
        }

        String longestMatch = isect.getStrings(AutomatonUtils.getLongestExample(isect)).iterator().next();
        //logger.info("longest match " + longestMatch);
        return longestMatch.length();
    }

    public double computeDiff(String test) {

        Automaton a = new RegExp(StringEscapeUtils.escapeJava(test)).toAutomaton();

        AutomatonTrans testa = new AutomatonTrans(a.clone());

        testa.convertToSubstringAutomaton();

        //logger.info(testa.toDot());

        AutomatonTrans testb = this.clone();
        testb.convertToSubstringAutomaton();
        Automaton isect = testa.intersection(testb);

        if(isect == null || isect.isEmpty() || isect.isEmptyString()) {
            return 0;
        }

        String longestMatch = isect.getStrings(AutomatonUtils.getLongestExample(isect)).iterator().next();
        //logger.info("longest match " + longestMatch);

        int sdiff = test.length() - longestMatch.length();

        //logger.info("get state boundaries");
        Set<StatePair> spair = getStateBoundaries(longestMatch);

        int val = -1;

        for(StatePair sp : spair) {

            //logger.info("first state " + getNumberOfState(sp.getFirstState()));
            //logger.info("first state " + getNumberOfState(sp.getSecondState()));


            int first = searchfromEndToStart(sp.getFirstState());
            int second = searchAcceptNodeFrom(sp.getSecondState());

            //logger.info("first " + first);
            //logger.info("second " + second);

            val = val < first + second && val >= 0 ? val : first + second;

        }

        //logger.info("val " + val);

        //logger.info("longest match length " + longestMatch.length());
        //logger.info("longest match length " + sdiff);

        //logger.info("first " + (double)longestMatch.length()/((double)test.length()+val) );
        return (double)longestMatch.length()/((double)test.length()+val);


    }

    private void setEpsilon() {

        boolean init = this.getInitialState().isAccept();

        Set<StatePair> spairs = new HashSet<StatePair>();
        for (State s : this.getStates()) {
            if (!s.equals(this.getInitialState())) {
                spairs.add(new StatePair(this.getInitialState(), s));
            }
        }
        this.addEpsilons(spairs);
        this.getInitialState().setAccept(init);
    }

    public void convertToCamelCaseAutomaton() {

        Set<Transition> handled = new HashSet<Transition>();

        for (State s : this.getStates()) {

            Set<Transition> transitions = new HashSet<Transition>();

            transitions.addAll(s.getTransitions());

            for (Transition t : transitions) {

                if (handled.contains(t))
                    continue;

                char min = t.getMin();
                char max = t.getMax();

                if (CharUtils.isLowerCase(min)) {
                    min = Character.toUpperCase(min);
                } else if (CharUtils.isUpperCase(min)) {
                    min = Character.toLowerCase(min);
                }

                if (CharUtils.isLowerCase(max)) {
                    max = Character.toUpperCase(min);
                } else if (CharUtils.isUpperCase(max)) {
                    max = Character.toLowerCase(min);
                }

                Transition tnew = new Transition(min, max, t.getDest());
                s.addTransition(tnew);
                handled.add(tnew);
                handled.add(t);
            }
        }

        this.removeDeadTransitions();
        this.determinize();
        this.reduce();
        this.kind = Kind.CAMEL;
        this.prepare();

    }

    public void convertToSubstringAutomaton() {
        setAccepting();
        setEpsilon();
        this.kind = Kind.SUBSTRING;
        this.finalize();
        this.prepare();
    }

    public void convertToSuffixAutomaton() {
        setEpsilon();
        this.kind = Kind.SUFFIX;
        this.finalize();
        this.prepare();
    }

    public void finalize() {
        dfsNumering(this.getInitialState());
    }

    private void dfsNumering(State s) {

        if (this.statenumber.containsKey(s))
            return;

        this.stateId++;
        this.statenumber.put(s, this.stateId);
        this.numberstate.put(this.stateId,s);
        for (Transition t : s.getTransitions()) {
            dfsNumering(t.getDest());
        }
    }

    static void appendCharString(char var0, StringBuilder var1) {
        if (var0 >= 33 && var0 <= 126 && var0 != 92 && var0 != 34) {
            var1.append(var0);
        } else {
            var1.append("\\u");
            String var2 = Integer.toHexString(var0);
            if (var0 < 16) {
                var1.append("000").append(var2);
            } else if (var0 < 256) {
                var1.append("00").append(var2);
            } else if (var0 < 4096) {
                var1.append("0").append(var2);
            } else {
                var1.append(var2);
            }
        }

    }

    void appendDot(StringBuilder sbuilder, Transition t) {
        sbuilder.append(" -> ").append(this.statenumber.get(t.getDest())).append(" [label=\"");
        appendCharString(t.getMin(), sbuilder);
        if (t.getMin() != t.getMax()) {
            sbuilder.append("-");
            appendCharString(t.getMax(), sbuilder);
        }

        sbuilder.append("\"]\n");
    }


    @Override
    public AutomatonTrans clone() {

        AutomatonTrans a = new AutomatonTrans();

        HashMap<State, State> m = new HashMap<State, State>();
        Set<State> states = this.getStates();

        for (State s : states)
            m.put(s, new State());

        for (State s : states) {
            State p = m.get(s);

            assert (p != null);
            p.setAccept(s.isAccept());

            if (s.equals(this.getInitialState())) {
                a.setInitialState(p);
                assert (a.getInitialState() != null);
                //logger.info("INITIAL STATE");
            }

            for (Transition t : s.getTransitions()) {
                p.getTransitions().add(new Transition(t.getMin(), t.getMax(), m.get(t.getDest())));
            }

            if (this.statenumber.containsKey(s)) {
                a.statenumber.put(p, this.statenumber.get(s));
            }
        }

        return a;
    }


    @Override
    public String toDot() {

        StringBuilder sbuilder = new StringBuilder("digraph Automaton {\n");
        sbuilder.append("  rankdir = LR;\n");
        Set states = this.getStates();

        Iterator stateIter = states.iterator();

        while (stateIter.hasNext()) {
            State state = (State) stateIter.next();
            sbuilder.append("  ").append(this.statenumber.get(state));
            if (state.isAccept()) {
                sbuilder.append(" [shape=doublecircle,label=\"" + this.statenumber.get(state) + "\"];\n");
            } else {
                sbuilder.append(" [shape=circle,label=\"" + this.statenumber.get(state) + "\"];\n");
            }

            if (state == this.getInitialState()) {
                sbuilder.append("  initial [shape=plaintext,label=\"" + this.statenumber.get(state) + "\"];\n");
                sbuilder.append("  initial -> ").append(this.statenumber.get(state)).append("\n");
            }

            Iterator transIter = state.getTransitions().iterator();

            while (transIter.hasNext()) {
                Transition trans = (Transition) transIter.next();
                sbuilder.append("  ").append(this.statenumber.get(state));
                appendDot(sbuilder, trans);
            }
        }

        return sbuilder.append("}\n").toString();
    }


    public boolean isSingleton() {
        return this.isSingleton();
    }

    public int getNumberOfState(State s) {
        if (this.statenumber.containsKey(s))
            return this.statenumber.get(s);
        else
            return -1;
    }
}

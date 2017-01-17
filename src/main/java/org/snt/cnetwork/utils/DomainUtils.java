package org.snt.cnetwork.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.*;

import java.util.*;

// @TODO make a signelton class out of this one
public class DomainUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(DomainUtils.class);

    private static int transId = 0;


    private static Character[] sarray = new Character[]{'+', '{', '}', '(', ')', '[', ']', '&', '^', '-', '?', '*', '\"', '$', '<', '>', '.', '|', '@', '#'};
    private static Set<Character> special = new HashSet<Character>(Arrays.asList(sarray));


    public static Automaton getLenAutomaton(NumCut min, NumCut max) {

        if(max.isFixed()) {
            return new Automaton(".{" + NumCut.max(new NumCut(0L), min).getEndpoint() +
                    "," +
                    max.getEndpoint() +
                    "}");
        } else {
            return new Automaton(".{" + NumCut.max(new NumCut(0L), min).getEndpoint() +
                    "," + "}");
        }
    }

    public static Automaton getAutomatonForRange(AtomicNumRange r) {
        return getLenAutomaton(r.getMin(), r.getMax());
    }

    public static Automaton getBoolAutomaton(BooleanRange r) {
        if(r.isAlwaysTrue()) {
            return new Automaton("[Tt][Rr][Uu][Ee]");
        } else if (r.isAlwaysFalse()) {
            return new Automaton("[Ff][Aa][Ll][Ss][Ee]");
        } else {
            return new Automaton("([Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee])");
        }
    }

    public static Automaton getAutomatonForRange(Range r) {
        if(r instanceof NumRange) {
            return getAutomatonForRange((NumRange)r);
        } else if (r instanceof BooleanRange) {
            return getAutomatonForRange((BooleanRange)r);
        } else if (r instanceof AtomicNumRange) {
            return getAutomatonForRange((AtomicNumRange)r);
        }
        // should never ever happen
        assert false;

        return null;
    }

    private static Automaton getAutomatonForRange(NumRange r) {
        Automaton a = new Automaton();
        r.getRangeMap().values().forEach(
                v -> a.union(getAutomatonForRange(v))
        );
        return a;
    }

    public static Automaton getNumAutomatonForRange(Range r) {
        return getNumAutomatonForRange(r.getMin(), r.getMax());
    }

    public static Automaton getBoolAutomatonForRange(BooleanRange r) {

        String strue = BoolCut.TRUE.getValue();
        String sfalse = BoolCut.FALSE.getValue();

        if(r.isAlwaysTrue()) {
            return new Automaton(strue);
        } else if (r.isAlwaysFalse()) {
            return new Automaton(sfalse);
        }

        return new Automaton(strue + "|" + sfalse);
    }

    public static Automaton getNumAutomatonForRange(long min, long max) {
        return getNumAutomatonForRange(new NumCut(min), new NumCut(max));
    }


    public static Automaton getNumAutomatonForRange(NumCut min, NumCut max) {

        assert max.isGreaterEqualsThan(min);

        if (max.equals(min)) {
            if(max.isFixed() && (max instanceof AboveAll))
                return new Automaton(min.toString());
        }
        String mins = "";
        String maxs = "";

        if(min.isFixed())
            mins = RexpUtils.getRexpForMin(min.getEndpoint());
        //LOGGER.info("MINS " + mins);
        if(max.isFixed())
            maxs = RexpUtils.getRexpForMax(max.getEndpoint());
        //LOGGER.info("MAXS " + maxs);

        Automaton mina = new Automaton(mins);
        Automaton maxa = new Automaton(maxs);


        return mina.intersect(maxa);
    }

    public static NumRange getApproxLenRange(Automaton a) {
        assert a != null;

        LOGGER.debug("get approx len range");
        LOGGER.debug("shortes " + a.getShortestExample());
        LOGGER.debug("finite " + a.isFinite());

        NumCut minlen = new NumCut(0L);
        NumCut maxlen = new AboveAll();

        if (!(a.isEmpty() || a.isEmptyString())) {
            minlen = new NumCut(a.getShortestExample().length());
        }

        if (a.isFinite()) {
        //    LOGGER.info("SHORTEST " + a.getShortestExample(true));
            maxlen = new NumCut(a.getLongestExample());
        }

        return new NumRange(new AtomicNumRange(minlen, maxlen));

    }

    public static NumRange getExactLenRange(Automaton a) {
        assert (a != null);

        Set<String> result = a.getFiniteStrings();

        List<AtomicNumRange> ar = new Vector();
        for(String s : result) {
            ar.add(new AtomicNumRange(s.length()));
        }

        return new NumRange(ar);
    }

    public static NumRange getNumRangeForAutomaton(Automaton a) {

        String r = NodeDomainFactory.Z_REXP;

        Automaton ra =  new Automaton(r);

        if(!ra.intersect(a).equals(a))
            return null;

        LOGGER.info(a.toString());

        Set<String> result = a.getFiniteStrings();

        Collection<AtomicNumRange> ar = new Vector<>();
        for(String s : result) {
            ar.add(new AtomicNumRange(Integer.parseInt(s)));
        }

        return new NumRange(ar);
    }




    
}

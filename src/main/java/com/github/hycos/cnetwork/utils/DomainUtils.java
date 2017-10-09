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

package com.github.hycos.cnetwork.utils;

import com.github.hycos.cnetwork.core.domain.NodeDomainFactory;
import com.github.hycos.cnetwork.core.domain.automaton.SimpleAutomaton;
import com.github.hycos.cnetwork.core.domain.range.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

// @TODO make a signelton class out of this one
public class DomainUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(DomainUtils.class);

    private static int transId = 0;


    private static Character[] sarray = new Character[]{'+', '{', '}', '(', ')', '[', ']', '&', '^', '-', '?', '*', '\"', '$', '<', '>', '.', '|', '@', '#'};
    private static Set<Character> special = new HashSet<Character>(Arrays.asList(sarray));


    public static SimpleAutomaton getLenAutomaton(NumCut min, NumCut max) {
        return new SimpleAutomaton(getLenStr(min,max));
    }

    public static String getLenStr(NumCut min, NumCut max) {

        String rexp = "";
        if(max.isFixed()) {
            rexp = ".{" + NumCut.max(new NumCut(0L), min).getEndpoint() +
                    "," +
                    max.getEndpoint() +
                    "}";
        } else {
            rexp = ".{" + NumCut.max(new NumCut(0L), min).getEndpoint() +
                    "," + "}";
        }

        return rexp;
    }




    public static SimpleAutomaton getBoolAutomatonForBoolRange(BooleanRange r) {
        if(r.isAlwaysTrue()) {
            return new SimpleAutomaton("[Tt][Rr][Uu][Ee]");
        } else if (r.isAlwaysFalse()) {
            return new SimpleAutomaton("[Ff][Aa][Ll][Ss][Ee]");
        } else {
            return new SimpleAutomaton("([Tt][Rr][Uu][Ee]|[Ff][Aa][Ll][Ss][Ee])");
        }
    }

    private static SimpleAutomaton getNumAutomatonForNumRange(NumRange r) {
        SimpleAutomaton a = new SimpleAutomaton();
        r.getRangeMap().values().forEach(
                v -> a.union(getNumAutomatonForRange(v))
        );
        return a;
    }


    public static SimpleAutomaton getNumAutomatonForRange(Range r) {

        if(r instanceof AtomicNumRange) {
            return getNumAutomatonForRange(r.getMin(), r.getMax());
        } else if (r instanceof BooleanRange) {
            return getBoolAutomatonForBoolRange((BooleanRange)r);
        } else if (r instanceof NumRange) {
            return getNumAutomatonForNumRange((NumRange)r);
        }
        assert false;
        return null;
    }

    public static SimpleAutomaton getBoolAutomatonForRange(BooleanRange r) {

        String strue = BooleanCut.TRUE.getValue();
        String sfalse = BooleanCut.FALSE.getValue();

        if(r.isAlwaysTrue()) {
            return new SimpleAutomaton(strue);
        } else if (r.isAlwaysFalse()) {
            return new SimpleAutomaton(sfalse);
        }

        return new SimpleAutomaton(strue + "|" + sfalse);
    }

    public static SimpleAutomaton getNumAutomatonForRange(long min, long max) {
        return getNumAutomatonForRange(new NumCut(min), new NumCut(max));
    }



    public static SimpleAutomaton getNumAutomatonForRange(NumCut min, NumCut max) {


        LOGGER.debug("get num auto");
        assert max.isGreaterEqualsThan(min);

        if (max.equals(min)) {
            if(max.isFixed() && (max instanceof AboveAll))
                return new SimpleAutomaton(min.toString());
        }
        String mins = "-?[1-9][0-9]+";
        String maxs = "-?[1-9][0-9]+";

        if(min.isFixed())
            mins = RexpUtils.getRexpForMin(min.getEndpoint());

        if(max.isFixed())
            maxs = RexpUtils.getRexpForMax(max.getEndpoint());

        LOGGER.info("MAXS " + maxs);
        LOGGER.info("MINS " + mins);

        SimpleAutomaton mina = new SimpleAutomaton(mins);
        SimpleAutomaton maxa = new SimpleAutomaton(maxs);


        return mina.intersect(maxa);
    }

    public static NumRange getApproxLenRange(SimpleAutomaton a) {
        assert a != null;

        //LOGGER.debug(a.toDot());

//        LOGGER.debug("get approx len range");
//        LOGGER.debug("shortes " + a.getShortestExample());
//        LOGGER.debug("finite " + a.isFinite());
//        LOGGER.debug("singleton " + a.isSingleton());

        NumCut minlen = new NumCut(0L);
        NumCut maxlen = new AboveAll();

        if (!(a.isEmpty() || a.isEmptyString())) {
            minlen = new NumCut(a.getShortestExample().length());
        }

        if (a.isFinite()) {
        //    LOGGER.info("SHORTEST " + a.getShortestExample(true));
            maxlen = new NumCut(a.getLongestExample());
        }

        if(a.isSingleton()){
            minlen = new NumCut(a.getShortestExample().length());
            maxlen = new NumCut(a.getShortestExample().length());
        }

        return new NumRange(new AtomicNumRange(minlen, maxlen));

    }

    public static NumRange getExactLenRange(SimpleAutomaton a) {
        assert (a != null);

        Set<String> result = a.getFiniteStrings();

        List<AtomicNumRange> ar = new Vector();
        for(String s : result) {
            ar.add(new AtomicNumRange(s.length()));
        }

        return new NumRange(ar);
    }

    public static NumRange getNumRangeForAutomaton(SimpleAutomaton a) {

        String r = NodeDomainFactory.Z_REXP;

        SimpleAutomaton ra =  new SimpleAutomaton(r);

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
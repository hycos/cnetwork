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

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.domain.range.AboveAll;
import com.github.hycos.cnetwork.core.domain.automaton.SimpleAutomaton;
import com.github.hycos.cnetwork.core.domain.range.NumCut;
import com.github.hycos.cnetwork.core.domain.range.NumRange;
import com.github.hycos.cnetwork.utils.DomainUtils;

import java.util.Set;


public class TestDomainUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(TestDomainUtils.class);
    private static int NR_RUNS = 10000;
    private static int ranges [][] = {{0, 100}, {1,999},{-2,-1}, {-45,678}};


    @Test
    public void testSimpleCases() {
        SimpleAutomaton a = DomainUtils.getNumAutomatonForRange(-1, 2);
        assert(a.run("1"));
        assert(a.run("0"));
    }

    @Test
    public void testGetRexpForRange() {
       for(int row [] : ranges ) {
           SimpleAutomaton a = DomainUtils.getNumAutomatonForRange(row[0], row[1]);
           //LOGGER.info(a.toDot());


           for(int i = row[0] - NR_RUNS ; i <= row[0] - 1; i++) {
               //LOGGER.info("[!Match " + i  + "]:Run with " + i);
               assert(!a.run(String.valueOf(i)));
           }
           for(int i = row[0] + 1; i < row[1]; i++) {
               //LOGGER.info("[Match " + i  + "]:Run with " + i);
               assert(a.run(String.valueOf(i)));
           }
           for(int i = row[1] + 1; i < row[1] + NR_RUNS; i++) {
               //LOGGER.info("[!Match " + i  + "]:Run with " + i);
               assert(!a.run(String.valueOf(i)));
           }
           //LOGGER.info(rexp);
       }
    }


   @Test
    public void testGetRangeForAutomaton() {

        NumRange range = DomainUtils.getApproxLenRange(new SimpleAutomaton
                ("aa+"));
        assert(range.isBetween(new NumCut(2),new AboveAll()));
        range = DomainUtils.getApproxLenRange(new SimpleAutomaton("[0-9]&[4-7]+"));
        assert(range.isBetween(1,1));
        range = DomainUtils.getApproxLenRange(new SimpleAutomaton("(gnt)*"));
        assert(range.isBetween(new NumCut(0),new AboveAll()));
        range = DomainUtils.getApproxLenRange(new SimpleAutomaton("(ab){0,10}cd"));
        LOGGER.info("RANGE " + range.toString());
        assert(range.isBetween(2,22));
        range = DomainUtils.getApproxLenRange(new SimpleAutomaton("(ab){0,10}c+d"));
        assert(range.isBetween(new NumCut(2),new AboveAll()));
        range = DomainUtils.getApproxLenRange(new SimpleAutomaton("(a|b){0,10}cd"));
        assert(range.isBetween(2,12));
    }

    @Test
    public void testRandomString() {

        SimpleAutomaton a = new SimpleAutomaton("ab*[0-1] ? ab");

        Set<String> set = a.getStrings(10);
        LOGGER.info("SIZ " + set.size());


        for(String s : set){
            LOGGER.info(s.toString());
        }
    }


}

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.AtomicNumRange;
import org.snt.cnetwork.core.domain.Automaton;
import org.snt.cnetwork.utils.DomainUtils;

import java.util.Set;


public class TestDomainUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(TestDomainUtils.class);
    private static int NR_RUNS = 10000;
    private static int ranges [][] = {{0, 100}, {1,999},{-2,-1}, {-45,678}};


    @Test
    public void testSimpleCases() {
        Automaton a = DomainUtils.getNumAutomatonForRange(-1, 2);
        assert(a.run("1"));
        assert(a.run("0"));
    }

    @Test
    public void testGetRexpForRange() {
       for(int row [] : ranges ) {
           Automaton a = DomainUtils.getNumAutomatonForRange(row[0], row[1]);
           //LOGGER.info(a.toDot());


           for(int i = row[0] - NR_RUNS ; i <= row[0]; i++) {
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

        AtomicNumRange range = DomainUtils.getApproxLenRange(new Automaton
                ("aa+"));
        assert(range.isBetween(2,Integer.MAX_VALUE));
        range = DomainUtils.getApproxLenRange(new Automaton("[0-9]&[4-7]+"));
        assert(range.isBetween(1,1));
        range = DomainUtils.getApproxLenRange(new Automaton("(gnt)*"));
        assert(range.isBetween(0,Integer.MAX_VALUE));
        range = DomainUtils.getApproxLenRange(new Automaton("(ab){0,10}cd"));
        LOGGER.info("RANGE " + range.toString());
        assert(range.isBetween(2,22));
        range = DomainUtils.getApproxLenRange(new Automaton("(ab){0,10}c+d"));
        assert(range.isBetween(2,Integer.MAX_VALUE));
        range = DomainUtils.getApproxLenRange(new Automaton("(a|b){0,10}cd"));
        assert(range.isBetween(2,12));
    }

    @Test
    public void testRandomString() {

        Automaton a = new Automaton("ab*[0-1] ? ab");

        Set<String> set = a.getStrings(10);
        LOGGER.info("SIZ " + set.size());


        for(String s : set){
            LOGGER.info(s.toString());
        }
    }


}

import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.utils.RexpUtils;


public class TestRexpUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(TestRexpUtils.class);

    private static int NR_RUNS = 10000;
    private static int limitsNeg [] = {-999, 0, -111, -222, -89679};
    private static int limitsPos [] = {999, 0, 111, 222, 89679};
    private static int ranges [][] = {{0, 100}, {1,999},{-2,-1}, {-45,678},{11,100}};

    @Test
    public void simpleTests() {


        String rexp = RexpUtils.getRexpForMax(100);
        RegExp automaton = new RegExp(rexp);

        Automaton a = automaton.toAutomaton();

        LOGGER.info("regular expression" + rexp);

    }

    @Test
    public void testGetRexpForMinPos() {

        for(int elem : limitsPos) {
            String rexp = RexpUtils.getRexpForMin(elem);
            RegExp automaton = new RegExp(rexp);

            Automaton a = automaton.toAutomaton();
            for (int i = 0; i < elem; i++) {
                //LOGGER.info("[!Match" + rexp + "]:Run with " + i);
                assert (!a.run(String.valueOf(i)));
            }

            for (int k = elem + 1; k <= elem + NR_RUNS; k++) {
                //LOGGER.info("[!Match" + rexp + "]:Run with " + k);
                assert (a.run(String.valueOf(k)));
            }
        }
    }

    @Test
    public void testGetRexpForMaxPos() {

        for(int elem : limitsPos) {
            String rexp = RexpUtils.getRexpForMax(elem);

            RegExp automaton = new RegExp(rexp);

            Automaton a = automaton.toAutomaton();
            for (int i = 0; i < elem; i++) {
                //LOGGER.info("[!Match " + "limit: " + elem + " " + rexp + "]:Run with " + i);
                assert (a.run(String.valueOf(i)));
            }

            for (int k = elem; k > elem + NR_RUNS; k++) {
                //LOGGER.info("[!Match" + rexp + "]:Run with " + k);
                assert (!a.run(String.valueOf(k)));
            }
        }
    }

    @Test
    public void testGetRexpForMinNeg() {

        for(int elem : limitsNeg) {
            String rexp = RexpUtils.getRexpForMin(elem);
            RegExp automaton = new RegExp(rexp);

            Automaton a = automaton.toAutomaton();
            for (int i = 0; i > elem; i--) {
                //LOGGER.info("[!Match " + i  + " " + rexp + "]:Run with " + i);
                assert (a.run(String.valueOf(i)));
            }

            for (int k = elem - 1; k > elem - NR_RUNS; k--) {
                //LOGGER.info("[!Match" + rexp + "]:Run with " + k);
                assert (!a.run(String.valueOf(k)));
            }
        }
    }

    @Test
    public void testGetRexpForMaxNeg() {

        for(int elem : limitsNeg) {
            String rexp = RexpUtils.getRexpForMax(elem);
            RegExp automaton = new RegExp(rexp);
            Automaton a = automaton.toAutomaton();
            for (int i = 0; i > elem; i--) {
                //LOGGER.info("[!Match " + elem  + " " + rexp + "]:Run with " + i);
                assert (!a.run(String.valueOf(i)));
            }

            for (int k = elem - 1; k > elem - NR_RUNS; k--) {
                //LOGGER.info("[!Match" + rexp + "]:Run with " + k);
                assert (a.run(String.valueOf(k)));
            }
        }
    }


    @Test
    public void testGetRexpForRange() {
        LOGGER.info("check range");
        for(int row [] : ranges ) {

            String rexp = RexpUtils.getRexpForRange(row[0], row[1]);

            LOGGER.info("Min " + row[0] + " Max " + row[1]  + " :" + rexp);

            RegExp automaton = new RegExp(rexp);
            Automaton a = automaton.toAutomaton();
            //LOGGER.info(a.toDot());

            for(int i = row[0] - NR_RUNS ; i <= row[0]; i++) {
                //LOGGER.info("[!Match " + i  + "]:Run with " + i);
                assert(!a.run(String.valueOf(i)));
            }
            for(int i = row[0] + 1; i < row[1]; i++) {
                //LOGGER.info("[Match " + i  + "]:Run with " + i);
                assert(a.run(String.valueOf(i)));
            }
            for(int i = row[1] + 1; i <= row[1] + NR_RUNS; i++) {
                //LOGGER.info("[!Match " + i  + "]:Run with " + i);
                assert(!a.run(String.valueOf(i)));
            }
            //LOGGER.info(rexp);

        }
    }

}

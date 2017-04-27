import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.automaton.SimpleAutomaton;

/**
 * Created by julian on 26/04/2017.
 */
public class TestMultimat {

    final static Logger LOGGER = LoggerFactory.getLogger(TestMultimat.class);

    @Test
    public void testMultimat() {

        SimpleAutomaton sa = new SimpleAutomaton(".{3,3}");
        SimpleAutomaton accept = new SimpleAutomaton(".*");

        SimpleAutomaton san = sa.concatenate(accept);

        LOGGER.debug("san {}", san.toDot());



    }
}

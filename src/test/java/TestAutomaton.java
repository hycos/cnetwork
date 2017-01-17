import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.Automaton;


public class TestAutomaton {

    final static Logger LOGGER = LoggerFactory.getLogger(TestAutomaton.class);

    @Test
    public void testsimple() {

        Automaton a0 = new Automaton("abcded");
        Assert.assertTrue(a0.isSingleton());

        LOGGER.debug(a0.getShortestExample());

        Automaton a1 = new Automaton("abcde+d");
        Assert.assertFalse(a1.isSingleton());

        LOGGER.debug(a1.getShortestExample());

    }

}


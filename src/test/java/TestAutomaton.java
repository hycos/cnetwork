import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.domain.automaton.SimpleAutomaton;


public class TestAutomaton {

    final static Logger LOGGER = LoggerFactory.getLogger(TestAutomaton.class);

    @Test
    public void testsimple() {

        SimpleAutomaton a0 = new SimpleAutomaton("abcded");
        Assert.assertTrue(a0.isSingleton());

        LOGGER.debug(a0.getShortestExample());

        SimpleAutomaton a1 = new SimpleAutomaton("abcde+d");
        Assert.assertFalse(a1.isSingleton());

        LOGGER.debug(a1.getShortestExample());

        SimpleAutomaton a3 = new SimpleAutomaton("");
        Assert.assertTrue(a3.isSingleton());
        Assert.assertTrue(a3.isEmptyString());


        SimpleAutomaton a4 = new SimpleAutomaton("hello w(orld)*");
        Assert.assertFalse(a4.isSingleton());
        Assert.assertFalse(a4.isEmptyString());

        SimpleAutomaton a5 = a4.intersect(new SimpleAutomaton("hello w"));
        Assert.assertTrue(a5.isSingleton());
        Assert.assertFalse(a5.isEmptyString());


    }

}


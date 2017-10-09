import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.graph.Operand;


public class TestBooleanRange {

    final static Logger LOGGER = LoggerFactory.getLogger(TestBooleanRange.class);

    @Test
    public void testBooleanRange() {
        Operand otrue = new Operand("true", NodeKind.BOOLLIT);
        Operand ofalse = new Operand("false", NodeKind.BOOLLIT);
        Operand ovar = new Operand("v3", NodeKind.BOOLVAR);

        LOGGER.info(otrue.toString());
        LOGGER.info(ofalse.toString());
        LOGGER.info(ovar.toString());
    }
}


import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.BooleanRange;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.core.Operand;


public class TestBooleanRange {

    final static Logger LOGGER = LoggerFactory.getLogger(TestBooleanRange.class);

    @Test
    public void testBooleanRange() {
        BooleanRange br = new BooleanRange();

        Operand otrue = new Operand("true", NodeKind.BOOLLIT);
        Operand ofalse = new Operand("false", NodeKind.BOOLLIT);
        Operand ovar = new Operand("v3", NodeKind.BOOLVAR);

        LOGGER.info(otrue.toString());
        LOGGER.info(ofalse.toString());
        LOGGER.info(ovar.toString());


    }
}

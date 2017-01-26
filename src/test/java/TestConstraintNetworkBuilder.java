import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.exception.EUFInconsistencyException;


public class TestConstraintNetworkBuilder {
    final static Logger LOGGER = LoggerFactory.getLogger(TestConstraintNetworkBuilder.class);

    @Test
    public void testBuilder() {

        ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder(true);

        Node x = new Operand("x", NodeKind.STRVAR);
        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, NodeKind.STRREXP);
        Node v1 = new Operand("sv7", NodeKind.NUMVAR);
        Node toStrV1 = cb.addOperation(NodeKind.TOSTR, v1);
        Node orv1 = cb.addOperation(NodeKind.CONCAT, or, toStrV1);
        Node eq = new Operand(" +\\>= +", NodeKind.STRREXP);
        Node orv1comp = cb.addOperation(NodeKind.CONCAT, orv1, eq);
        Node v2 = new Operand("sv8", NodeKind.NUMVAR);
        Node toStrV2 = cb.addOperation(NodeKind.TOSTR, v2);
        Node orv1compv2 = cb.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);
        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, NodeKind.STRREXP);

        try {
            cb.addOperation(NodeKind.CONCAT, orv1compv2, comment);
            cb.addConstraint(NodeKind.NUM_EQUALS, v1, v2);
            cb.addConstraint(NodeKind.MATCHES, x, orv1compv2);
        } catch (EUFInconsistencyException e) {
            LOGGER.error(e.getMessage());
        }
        LOGGER.debug(cb.getConstraintNetwork().toDot());
        LOGGER.debug(cb.getEufLattice().toDot());

    }


}

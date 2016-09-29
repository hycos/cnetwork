
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;


public class TestConstraintNetwork {

    final static Logger LOGGER = LoggerFactory.getLogger(TestConstraintNetwork.class);


    @Test
    public void testCnConstruction() {

        ConstraintNetwork tm2 = new ConstraintNetwork();
        Node x = new Operand("x", OperandKind.STRVAR);
        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, OperandKind.STRREXP);

        Node v1 = new Operand("sv7", OperandKind.NUMVAR);

        Node toStrV1 = tm2.addOperation(OperationKind.TOSTR, v1);

        Node orv1 = tm2.addOperation(OperationKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" +\\>= +", OperandKind.STRREXP);

        Node orv1comp = tm2.addOperation(OperationKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv8", OperandKind.NUMVAR);

        Node toStrV2 = tm2.addOperation(OperationKind.TOSTR, v2);

        Node orv1compv2 = tm2.addOperation(OperationKind.CONCAT, orv1comp, toStrV2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, OperandKind.STRREXP);

        tm2.addOperation(OperationKind.CONCAT,orv1compv2,comment);

        tm2.addConstraint(OperationKind.GREATEREQ, v1,v2);

        tm2.setStartNode(orv1compv2);
        tm2.addConstraint(OperationKind.MATCHES, x, orv1compv2);

        LOGGER.info(tm2.toDot());
    }


    @Test
    public void testCNClone() {
        ConstraintNetwork tm2 = new ConstraintNetwork();
        Node x = new Operand("x", OperandKind.STRVAR);
        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, OperandKind.STRREXP);
        Node v1 = new Operand("sv7", OperandKind.NUMVAR);
        Node toStrV1 = tm2.addOperation(OperationKind.TOSTR, v1);
        Node orv1 = tm2.addOperation(OperationKind.CONCAT, or, toStrV1);
        Node eq = new Operand(" +\\>= +", OperandKind.STRREXP);
        Node orv1comp = tm2.addOperation(OperationKind.CONCAT, orv1, eq);
        Node v2 = new Operand("sv8", OperandKind.NUMVAR);
        Node toStrV2 = tm2.addOperation(OperationKind.TOSTR, v2);
        Node orv1compv2 = tm2.addOperation(OperationKind.CONCAT, orv1comp, toStrV2);
        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, OperandKind.STRREXP);
        tm2.addOperation(OperationKind.CONCAT,orv1compv2,comment);
        tm2.addConstraint(OperationKind.GREATEREQ, v1,v2);
        tm2.setStartNode(orv1compv2);
        tm2.addConstraint(OperationKind.MATCHES, x, orv1compv2);

        LOGGER.info(tm2.toDot());
        ConstraintNetwork tm3 = tm2.clone();
        LOGGER.info(tm3.toDot());

    }


}



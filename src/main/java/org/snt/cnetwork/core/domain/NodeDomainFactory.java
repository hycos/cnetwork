package org.snt.cnetwork.core.domain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.utils.DomainUtils;

public class NodeDomainFactory {
    final static Logger LOGGER = LoggerFactory.getLogger(NodeDomainFactory.class);

    private static NodeDomainFactory factory = null;

    public static final String Z_REXP = "-?([0-9]|[1-9][0-9]{0,7})";
    public static final String N_REXP = "[0-9]|([1-9][0-9]{0,7})";
    public static final String STR_REXP = ".*";
    public static final String STR_REXP_LOWER = "[^A-Z]*";
    public static final String STR_REXP_UPPER = "[^a-z]*";
    public static final String STR_REXP_TRIMMED = "[^ ]*.*[^ ]*";
    public static final String BOOL_REXP = "0|1";


    public static NumRange N = new NumRange(AtomicNumRange.N);
    public static NumRange Z = new NumRange(AtomicNumRange.Z);

    public static BooleanRange FALSE = new BooleanRange(BooleanRange.BooleanValue.FALSE);
    public static BooleanRange TRUE = new BooleanRange(BooleanRange.BooleanValue.TRUE);


    public static NodeDomainFactory getInstance() {
        if (factory == null)
            factory = new NodeDomainFactory();
        return factory;
    }

    private NodeDomainFactory() {
    }

    public NodeDomain getDomain(Node n) {
        return getDomain(n.getKind(), n.getLabel());
    }

    public NodeDomain getDomain(NodeKind n) {
        return getDomain(n,null);
    }

    public NodeDomain getDomain(NodeKind n, String lbl) {
        switch (n.getDomainKind()) {
            case UNKNOWN:
                return new NodeDomain(new Automaton(STR_REXP),
                        Z.clone());
            case NUMERIC_Z:
                return new NodeDomain(new Automaton(Z_REXP),
                        Z.clone());
            case NUMERIC_N:
                assert n.isNumeric();
                if (n.isLiteral()) {
                    assert lbl != null & lbl.length() > 0;
                    assert lbl.matches(N_REXP);
                    int value = Integer.parseInt(lbl);
                    return new NodeDomain(new Automaton(lbl), new NumRange(value));
                } else {

                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return new NodeDomain(new Automaton(N_REXP),
                            N.clone());
                }
            case STRING:
                if ((n.isLiteral() || n.isRegex())) {

                    assert lbl != null;

                    Automaton a = new Automaton(lbl);
                    return new NodeDomain(a, DomainUtils
                            .getApproxLenRange(a));
                } else {
                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return new NodeDomain(new Automaton(STR_REXP),
                            N.clone());
                }
            case STRING_UPPER:
                return new NodeDomain(new Automaton(STR_REXP_UPPER), N.clone());
            case STRING_LOWER:
                return new NodeDomain(new Automaton(STR_REXP_LOWER), N.clone());
            case STRING_TRIMMED:
                return new NodeDomain(new Automaton(STR_REXP_TRIMMED), N.clone());
            case BOOLEAN:
                if (n.isLiteral()) {
                    LOGGER.debug("__" + lbl);

                    assert lbl != null && lbl.matches("(true|false)");
                    BooleanRange.BooleanValue bv = BooleanRange.BooleanValue
                            .KindFromString(lbl);

                    return new NodeDomain(new Automaton(bv.getValue()), new
                            BooleanRange(bv));
                } else {
                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return new NodeDomain(new Automaton(BOOL_REXP),
                            new BooleanRange());
                }
        }

        // should never ever happen;
        assert false;
        return null;
    }

}

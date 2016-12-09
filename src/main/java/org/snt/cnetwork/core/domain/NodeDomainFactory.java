package org.snt.cnetwork.core.domain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.utils.DomainUtils;
import org.snt.cnetwork.utils.EscapeUtils;

import java.util.HashMap;
import java.util.Map;

public enum NodeDomainFactory {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(NodeDomainFactory.class);

    public static final String Z_REXP = "-?([0-9]|[1-9][0-9]{0,7})";
    public static final String N_REXP = "[0-9]|([1-9][0-9]{0,7})";
    public static final String STR_REXP = ".*";
    public static final String STR_REXP_LOWER = "[^A-Z]*";
    public static final String STR_REXP_UPPER = "[^a-z]*";
    public static final String STR_REXP_TRIMMED = "[^ ]*.*[^ ]*";
    public static final String BOOL_TRUE = "[Tt][Rr][Uu][Ee]";
    public static final String BOOL_FALSE = "[Ff][Aa][Ll][Ss][Ee]";
    public static final String BOOL_REXP = "(" + BOOL_TRUE + "|" + BOOL_FALSE
            + ")";


    public static NumRange N = new NumRange(AtomicNumRange.N);
    public static NumRange Z = new NumRange(AtomicNumRange.Z);

    public static BooleanRange FALSE = new BooleanRange(BooleanRange.BooleanValue.FALSE);
    public static BooleanRange TRUE = new BooleanRange(BooleanRange.BooleanValue.TRUE);


    public static NodeDomain DB = new NodeDomain(DomainKind.BOOLEAN,
            new Automaton(BOOL_REXP),
            new BooleanRange());

    public static NodeDomain DBTRUE = new NodeDomain(DomainKind.BOOLEAN,
            new Automaton(BOOL_TRUE),
            TRUE.clone());

    public static NodeDomain DBFALSE = new NodeDomain(DomainKind.BOOLEAN,
            new Automaton(BOOL_FALSE),
            FALSE.clone());

    public static NodeDomain DN = new NodeDomain(DomainKind.NUMERIC_N,
            new Automaton(N_REXP), N.clone());

    public static NodeDomain DZ = new NodeDomain(DomainKind.NUMERIC_Z,
            new Automaton(Z_REXP), N.clone());

    public static NodeDomain DSTR = new NodeDomain(DomainKind.STRING,new Automaton
            (STR_REXP), N.clone());

    public static NodeDomain DSTRU = new NodeDomain(DomainKind.STRING_UPPER,new
            Automaton (STR_REXP_UPPER), N.clone());

    public static NodeDomain DSTRL = new NodeDomain(DomainKind.STRING_LOWER,new
            Automaton (STR_REXP_UPPER), N.clone());

    public static NodeDomain DSTRT = new NodeDomain(DomainKind.STRING_TRIMMED,new
            Automaton (STR_REXP_TRIMMED), N.clone());

    public static Map<DomainKind, NodeDomain> dkindLookup = new HashMap();


    static {
        dkindLookup.put(DomainKind.NUMERIC_LZ, DZ);
        dkindLookup.put(DomainKind.NUMERIC_Z, DZ);
        dkindLookup.put(DomainKind.NUMERIC_N, DN);
        dkindLookup.put(DomainKind.STRING, DSTR);
        dkindLookup.put(DomainKind.BOOLEAN, DB);
        dkindLookup.put(DomainKind.STRING_UPPER, DSTRU);
        dkindLookup.put(DomainKind.STRING_LOWER, DSTRL);
        dkindLookup.put(DomainKind.STRING_TRIMMED, DSTRT);

    }


    public NodeDomain getDomain(Node n) {
        return getDomain(n.getKind(), n.getLabel());
    }


    public NodeDomain getDomain(DomainKind kind) {
        switch (kind) {
            case UNKNOWN:
                break;
            case NUMERIC_Z:
                return DZ.clone();
            case NUMERIC_N:
                return DN.clone();
            case NUMERIC_LZ:
                return DN.clone();
            case STRING:
                return DSTR.clone();
            case STRING_UPPER:
                return DSTRU.clone();
            case STRING_LOWER:
                return DSTRL.clone();
            case STRING_TRIMMED:
               return DSTRT.clone();
            case BOOLEAN:
                return DB.clone();
        }
        assert false;
        return null;

    }

    public NodeDomain getDomain(NodeKind n) {
        return getDomain(n,null);
    }


    public NodeDomain getDomain(NodeKind n, String lbl) {

        LOGGER.debug("getDomain " + n.getDomainKind() + " " + lbl);

        switch (n.getDomainKind()) {
            case UNKNOWN:
                return new NodeDomain(n.getDomainKind(),new Automaton(STR_REXP),
                        Z.clone());
            case NUMERIC_Z:
            case NUMERIC_LZ:
            case NUMERIC_N:
                assert n.isNumeric();
                if (n.isLiteral()) {
                    assert lbl != null & lbl.length() > 0;
                    assert lbl.matches(N_REXP);
                    int value = Integer.parseInt(lbl);
                    return new NodeDomain(n.getDomainKind(),
                            new Automaton(lbl),
                            new NumRange (value));
                } else {
                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return dkindLookup.get(n.getDomainKind());
                }
            case STRING:
                assert lbl != null;
                if (n.isLiteral()) {
                    Automaton a = new Automaton(EscapeUtils.escapeSpecialCharacters(lbl));
                    return new NodeDomain(DomainKind.STRING,
                            a,
                            DomainUtils.getApproxLenRange(a));
                } else if (n.isRegex()) {
                    Automaton a = new Automaton(lbl);
                    return new NodeDomain(DomainKind.STRING,
                            a,
                            DomainUtils.getApproxLenRange(a));
                } else {
                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return new NodeDomain(n.getDomainKind(),new Automaton
                            (STR_REXP),
                            N.clone());
                }
            case STRING_UPPER:
                return new NodeDomain(n.getDomainKind(),new Automaton
                        (STR_REXP_UPPER), N.clone());
            case STRING_LOWER:
                return new NodeDomain(n.getDomainKind(),new Automaton
                        (STR_REXP_LOWER), N.clone());
            case STRING_TRIMMED:
                return new NodeDomain(n.getDomainKind(),new Automaton
                        (STR_REXP_TRIMMED), N
                        .clone());
            case BOOLEAN:
                if (n.isLiteral()) {
                    LOGGER.debug("__" + lbl);

                    assert lbl != null && lbl.matches("(true|false)");
                    BooleanRange.BooleanValue bv = BooleanRange.BooleanValue
                            .KindFromString(lbl);

                    return new NodeDomain(n.getDomainKind(),new Automaton(bv
                            .getValue()), new
                            BooleanRange(bv));
                } else {
                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return DB.clone();
                }
        }

        // should never ever happen;
        assert false;
        return null;
    }

}

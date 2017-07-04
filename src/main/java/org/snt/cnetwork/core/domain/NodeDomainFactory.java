package org.snt.cnetwork.core.domain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.automaton.SimpleAutomaton;
import org.snt.cnetwork.core.domain.range.*;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.core.graph.NodeKind;
import org.snt.cnetwork.utils.DomainUtils;
import org.snt.cnetwork.utils.EscapeUtils;

import java.util.HashMap;
import java.util.Map;

public enum NodeDomainFactory {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(NodeDomainFactory.class);

    public static final String Z_REXP = "-?([0-9]|[1-9][0-9]{0,7})";
    public static final String N_REXP = "[0-9]|([1-9][0-9]{0,7})";
    public static final String NM1_REXP = N_REXP + "|-1";
    public static final String STR_REXP = ".*";
    public static final String STR_REXP_LOWER = "[^A-Z]*";
    public static final String STR_REXP_UPPER = "[^a-z]*";
    public static final String STR_REXP_TRIMMED = "[^ ].*[^ ]*";
    public static final String BOOL_TRUE = "[Tt][Rr][Uu][Ee]";
    public static final String BOOL_FALSE = "[Ff][Aa][Ll][Ss][Ee]";
    public static final String BOOL_REXP = "(" + BOOL_TRUE + "|" + BOOL_FALSE
            + ")";

    public static final String STR_EMPTY = ".{0}";

    public static NumRange N = new NumRange(AtomicNumRange.N.clone());
    public static NumRange Z = new NumRange(AtomicNumRange.Z.clone());
    public static NumRange E = new NumRange(AtomicNumRange.E.clone());
    public static NumRange NM1 = new NumRange(new AtomicNumRange(new NumCut
            (-1), new AboveAll()));

    public static BooleanRange FALSE = new BooleanRange(BooleanCut.FALSE.clone());
    public static BooleanRange TRUE = new BooleanRange(BooleanCut.TRUE.clone());


    public static NodeDomain DB = new NodeDomain(DomainKind.BOOLEAN,
            new SimpleAutomaton(BOOL_REXP),
            new BooleanRange());

    public static NodeDomain DBTRUE = new NodeDomain(DomainKind.BOOLEAN,
            new SimpleAutomaton(BOOL_TRUE),
            TRUE.clone());

    public static NodeDomain DBFALSE = new NodeDomain(DomainKind.BOOLEAN,
            new SimpleAutomaton(BOOL_FALSE),
            FALSE.clone());

    public static NodeDomain DN = new NodeDomain(DomainKind.NUMERIC_N, new
            SimpleAutomaton(N_REXP), N.clone());

    public static NodeDomain DNM1 = new NodeDomain(DomainKind.NUMERIC_NM1,
            new SimpleAutomaton(NM1_REXP),
            NM1.clone());

    public static NodeDomain DZ = new NodeDomain(DomainKind.NUMERIC_Z,
            new SimpleAutomaton(Z_REXP), Z.clone());

    public static NodeDomain DSTR = new NodeDomain(DomainKind.STRING,new SimpleAutomaton
            (STR_REXP), N.clone());

    public static NodeDomain DSTRU = new NodeDomain(DomainKind.STRING_UPPER,new
            SimpleAutomaton(STR_REXP_UPPER), N
            .clone());

    public static NodeDomain DSTRL = new NodeDomain(DomainKind.STRING_LOWER,new
            SimpleAutomaton(STR_REXP_LOWER), N.clone());

    public static NodeDomain DSTRT = new NodeDomain(DomainKind.STRING_TRIMMED,new
            SimpleAutomaton(STR_REXP_TRIMMED), N.clone());

    public static NodeDomain DSTRE = new NodeDomain(DomainKind
            .STRING,new
            SimpleAutomaton(STR_EMPTY), E.clone());

    public static Map<DomainKind, NodeDomain> dkindLookup = new HashMap();


    static {
        dkindLookup.put(DomainKind.NUMERIC_NM1, DNM1);
        dkindLookup.put(DomainKind.NUMERIC_Z, DZ);
        dkindLookup.put(DomainKind.NUMERIC_N, DN);
        dkindLookup.put(DomainKind.STRING, DSTR);
        dkindLookup.put(DomainKind.BOOLEAN, DB);
        dkindLookup.put(DomainKind.STRING_UPPER, DSTRU);
        dkindLookup.put(DomainKind.STRING_LOWER, DSTRL);
        dkindLookup.put(DomainKind.STRING_TRIMMED, DSTRT);
    }


    public NodeDomain getDomain(Node n) {
        NodeDomain nd = getDomain(n.getKind(), n.getLabel());

        //@TODO: Julian this is damn ugly
        if(n.isLiteral() && n.isString())
            n.setLabel("\"" + n.getLabel() + "\"");


        return nd;
    }


    public NodeDomain getDomain(DomainKind kind) {
        LOGGER.debug("get domain for kind {}", kind);
        switch (kind) {
            case UNKNOWN:
                return DSTR.clone();
            case NUMERIC_Z:
                return DZ.clone();
            case NUMERIC_N:
                return DN.clone();
            case NUMERIC_NM1:
                return DNM1.clone();
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

    public NodeDomain getDomainForKind(NodeKind n) {

        if(n == NodeKind.STRVAR) {
            return getDomain(n, ".*");
        }
        return getDomain(n, null);
    }



    public NodeDomain getDomain(NodeKind n, String lbl) {


        LOGGER.debug("getDomainForKind " + n.getDomainKind() + " ");

        switch (n.getDomainKind()) {
            case NUMERIC_Z:
            case NUMERIC_N:
            case NUMERIC_NM1:
                assert n.isNumeric();
                if (n.isLiteral()) {
                    assert lbl != null & lbl.length() > 0;
                    assert lbl.matches(Z_REXP);
                    int value = Integer.parseInt(lbl);
                    return new NodeDomain(n.getDomainKind(),
                            new SimpleAutomaton(lbl),
                            new NumRange (value));
                } else {
                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return getDomain(n.getDomainKind());
                }
            case STRING:
                assert lbl != null;
                if (n.isLiteral()) {
                    SimpleAutomaton a = new SimpleAutomaton(EscapeUtils.escapeSpecialCharacters(lbl));
                    return new NodeDomain(DomainKind.STRING,
                            a,
                            DomainUtils.getApproxLenRange(a));
                } else if (n.isRegex()) {
                    SimpleAutomaton a = new SimpleAutomaton(lbl);
                    return new NodeDomain(DomainKind.STRING,
                            a,
                            DomainUtils.getApproxLenRange(a));
                } else {
                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return getDomain(n.getDomainKind());
                }
            case UNKNOWN:
            case STRING_UPPER:
            case STRING_LOWER:
            case STRING_TRIMMED:
                return getDomain(n.getDomainKind());
            case BOOLEAN:
                if (n.isLiteral()) {
                    LOGGER.debug("__" + lbl);

                    assert lbl != null && lbl.matches("(true|false)");
                    BooleanCut bv = BooleanCut.KindFromString(lbl);

                    return new NodeDomain(n.getDomainKind(),new SimpleAutomaton(bv
                            .getValue()), new
                            BooleanRange(bv));
                } else {
                    assert n.isOperation() || n.isVariable() || n.isRegex();
                    return getDomain(n.getDomainKind());
                }
        }

        // should never ever happen;
        assert false;
        return null;
    }

}

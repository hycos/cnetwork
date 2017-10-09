/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetwork.core.domain;

import com.github.hycos.cnetwork.core.domain.automaton.SimpleAutomaton;
import com.github.hycos.cnetwork.core.domain.range.*;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.utils.EscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.utils.DomainUtils;

import java.util.HashMap;
import java.util.Map;

import static com.github.hycos.cnetwork.core.domain.DomainKind.*;
import static com.github.hycos.cnetwork.core.domain.DomainKind.NUMERIC_Z;

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


    public static NodeDomain DB = new NodeDomain(BOOLEAN,
            new SimpleAutomaton(BOOL_REXP),
            new BooleanRange());

    public static NodeDomain DBTRUE = new NodeDomain(BOOLEAN,
            new SimpleAutomaton(BOOL_TRUE),
            TRUE.clone());

    public static NodeDomain DBFALSE = new NodeDomain(BOOLEAN,
            new SimpleAutomaton(BOOL_FALSE),
            FALSE.clone());

    public static NodeDomain DN = new NodeDomain(NUMERIC_N, new
            SimpleAutomaton(N_REXP), N.clone());

    public static NodeDomain DNM1 = new NodeDomain(NUMERIC_NM1,
            new SimpleAutomaton(NM1_REXP),
            NM1.clone());

    public static NodeDomain DZ = new NodeDomain(NUMERIC_Z,
            new SimpleAutomaton(Z_REXP), Z.clone());

    public static NodeDomain DSTR = new NodeDomain(STRING,new SimpleAutomaton
            (STR_REXP), N.clone());

    public static NodeDomain DSTRU = new NodeDomain(STRING_UPPER,new
            SimpleAutomaton(STR_REXP_UPPER), N
            .clone());

    public static NodeDomain DSTRL = new NodeDomain(STRING_LOWER,new
            SimpleAutomaton(STR_REXP_LOWER), N.clone());

    public static NodeDomain DSTRT = new NodeDomain(STRING_TRIMMED,new
            SimpleAutomaton(STR_REXP_TRIMMED), N.clone());

    public static NodeDomain DSTRE = new NodeDomain(
            STRING,new
            SimpleAutomaton(STR_EMPTY), E.clone());

    public static Map<DomainKind, NodeDomain> dkindLookup = new HashMap();


    static {
        dkindLookup.put(NUMERIC_NM1, DNM1);
        dkindLookup.put(NUMERIC_Z, DZ);
        dkindLookup.put(NUMERIC_N, DN);
        dkindLookup.put(STRING, DSTR);
        dkindLookup.put(BOOLEAN, DB);
        dkindLookup.put(STRING_UPPER, DSTRU);
        dkindLookup.put(STRING_LOWER, DSTRL);
        dkindLookup.put(STRING_TRIMMED, DSTRT);
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
                    return new NodeDomain(STRING,
                            a,
                            DomainUtils.getApproxLenRange(a));
                } else if (n.isRegex()) {
                    SimpleAutomaton a = new SimpleAutomaton(lbl);
                    return new NodeDomain(STRING,
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

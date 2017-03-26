package org.snt.cnetwork.core.consistency;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.core.consistency.general.*;
import org.snt.cnetwork.core.consistency.specific.*;

public enum ConsistencyCheckerFactory {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(ConsistencyCheckerFactory.class);

    private static ConsistencyChecker ok = new Ok();
    private static ConsistencyChecker bbo = new BooleanBinaryOp();
    private static ConsistencyChecker buo = new BooleanUnaryOp();
    private static ConsistencyChecker nbo = new NumericBinaryOp();
    private static ConsistencyChecker nco = new NumericCompOp();
    private static ConsistencyChecker sbo = new StringBinaryOp();
    private static ConsistencyChecker suo = new StringUnaryOp();
    private static ConsistencyChecker eq = new Equals();
    private static ConsistencyChecker ite = new Ite();
    private static ConsistencyChecker charat = new CharAt();
    private static ConsistencyChecker contains = new Contains();
    private static ConsistencyChecker emtpy = new Empty();
    private static ConsistencyChecker len = new Len();
    private static ConsistencyChecker matches = new Matches();
    private static ConsistencyChecker replace = new Replace();
    private static ConsistencyChecker startsWith = new StartsWith();
    private static ConsistencyChecker substr = new Substr();
    private static ConsistencyChecker tostr = new ToStr();
    private static ConsistencyChecker valueof = new ValueOf();
    private static ConsistencyChecker idxof = new IndexOf();


    public boolean isConsistent(ConstraintNetworkBuilder cb, Node n) {
        return getConsistencyCheckerFor(n.getKind()).check(cb, n);
    }

    public ConsistencyChecker getConsistencyCheckerFor(NodeKind kind){

        LOGGER.debug("kind {}", kind);

        switch (kind) {

            case UNKNOWN:
            case EXTERNAL:
            case SEARCH:
                return ok;

            case OR:
            case AND:
            case XOR:
            case IMPLIES:
               return bbo;

            case NOT:
                return buo;

            case ITE:
                return ite;

            case NUMVAR:
            case STRVAR:
            case STRREXP:
            case NUMLIT:
            case STRLIT:
            case BOOLLIT:
            case BOOLVAR:
            case XMLI:
            case SQLISTR:
            case SQLINUM:
            case XPATHSTR:
            case XPATHNUM:
            case LDAPI:
            case XSS:
            case URLI:
                return ok;

            case EQUALS:
            case STR_EQUALS:
            case NUM_EQUALS:
            case BOOL_EQUALS:
            case STR_EQUALSIC:
            case NEQUALS:
            case STR_NEQUALS:
            case NUM_NEQUALS:
            case BOOL_NEQUALS:
            case STR_NEQUALSIC:
                return eq;

            case SMALLER:
            case GREATER:
            case SMALLEREQ:
            case GREATEREQ:
                return nco;

            case ADD:
            case SUB:
            case DIV:
                return nbo;

            case MATCHES:
                return matches;

            case STARTSWITH:
            case ENDSWITH:
                return startsWith;

            case CONTAINS:
                return contains;

            case SUBSTR:
                return substr;

            case INDEXOF:
                return idxof;

            case TOLOWER:
            case TOUPPER:
            case TRIM:
            case STRINV:
            case APACHE_ESCHTML:
            case APACHE_UESCHTML:
            case APACHE_ESCXML10:
            case APACHE_ESCXML11:
            case APACHE_ESCJSON:
            case APACHE_ESCECMA:
            case ESAPI_ESCLDAP:
            case ESAPI_ESCDN:
            case ESAPI_ESCHTML:
            case ESAPI_ESCHTMLATTR:
            case ESAPI_ESCXML:
            case ESAPI_ESCXMLATTR:
            case ESAPI_ESCXPATH:
            case ESAPI_ESCSQL:
                return suo;

            case EMTPY:
                return emtpy;

            case CONCAT:
                return sbo;

            case LEN:
                return len;

            case REPLACE:
                return replace;

            case CHARAT:
                return charat;

            case VALUEOF:
                return valueof;

            case TOSTR:
                return tostr;


        }

        // should never be reached
        assert false;
        return ok;
    }


}

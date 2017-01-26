package org.snt.cnetwork.core;


import org.snt.cnetwork.core.domain.DomainKind;
import org.snt.cnetwork.exception.IllegalDomainException;

public enum NodeKind {

    UNKNOWN("unkown", DomainKind.UNKNOWN),

    // Operands
    NUMVAR("numvar", DomainKind.NUMERIC_Z),
    STRVAR("strvar", DomainKind.STRING),
    STRREXP("strexp", DomainKind.STRING),
    NUMLIT("numlit", DomainKind.NUMERIC_Z),
    STRLIT("strlit", DomainKind.STRING),
    BOOLLIT("boollit", DomainKind.BOOLEAN),
    BOOLVAR("boolvar", DomainKind.BOOLEAN),

    XMLI("xmli", DomainKind.STRING),
    SQLISTR("sqlistr", DomainKind.STRING),
    SQLINUM("sqlinum", DomainKind.STRING),
    XPATHSTR("xpathstr", DomainKind.STRING),
    XPATHNUM("xpathnum", DomainKind.STRING),
    LDAPI("ldapi", DomainKind.STRING),
    XSS("xss", DomainKind.STRING),
    URLI("urli", DomainKind.STRING),

    // Operations
    EXTERNAL("external", DomainKind.STRING),
    // overloaded operators
    EQUALS("==", DomainKind.BOOLEAN),
    STR_EQUALS("==", DomainKind.BOOLEAN),
    NUM_EQUALS("==", DomainKind.BOOLEAN),
    BOOL_EQUALS("==", DomainKind.BOOLEAN),
    STR_EQUALSIC("~~", DomainKind.BOOLEAN),

    NEQUALS("!=", DomainKind.BOOLEAN),
    STR_NEQUALS("!=", DomainKind.BOOLEAN),
    NUM_NEQUALS("!=", DomainKind.BOOLEAN),
    BOOL_NEQUALS("!=", DomainKind.BOOLEAN),
    STR_NEQUALSIC("!~", DomainKind.BOOLEAN),


    SMALLER("<", DomainKind.BOOLEAN),
    GREATER(">", DomainKind.BOOLEAN),
    SMALLEREQ("<=", DomainKind.BOOLEAN),
    GREATEREQ(">=", DomainKind.BOOLEAN),
    MATCHES("matches", DomainKind.BOOLEAN),
    STARTSWITH("startswith", DomainKind.BOOLEAN),
    ENDSWITH("endswith", DomainKind.BOOLEAN),
    CONTAINS("contains", DomainKind.BOOLEAN),

    OR("or", DomainKind.BOOLEAN),
    AND("and", DomainKind.BOOLEAN),
    XOR("xor", DomainKind.BOOLEAN),
    NOT("not", DomainKind.BOOLEAN),
    ITE("ite", DomainKind.BOOLEAN, true),
    IMPLIES("implies", DomainKind.BOOLEAN),

    SUBSTR("substr", DomainKind.STRING),
    INDEXOF("indexof", DomainKind.NUMERIC_NM1),
    TOLOWER("tolower", DomainKind.STRING_LOWER),
    TOUPPER("toupper", DomainKind.STRING_UPPER),
    CONCAT("concat", DomainKind.STRING),
    TRIM("trim", DomainKind.STRING),
    LEN("len", DomainKind.NUMERIC_N),
    REPLACE("replace", DomainKind.STRING),
    STRINV("strinv", DomainKind.STRING),
    CHARAT("charat", DomainKind.STRING),

    VALUEOF("valueof", DomainKind.NUMERIC_Z),
    TOSTR("tostr", DomainKind.STRING),

    ADD("add", DomainKind.NUMERIC_Z),
    SUB("sub", DomainKind.NUMERIC_Z),

    APACHE_ESCHTML("apache_eschtml", DomainKind.STRING),
    APACHE_UESCHTML("apache_ueschtml", DomainKind.STRING),
    APACHE_ESCXML10("apache_escxml10", DomainKind.STRING),
    APACHE_ESCXML11("apache_escxml11", DomainKind.STRING),
    APACHE_ESCJSON("apache_escjson", DomainKind.STRING),
    APACHE_ESCECMA("apache_escecma", DomainKind.STRING),

    ESAPI_ESCLDAP("esapi_escldap", DomainKind.STRING),
    ESAPI_ESCDN("esapi_escdn", DomainKind.STRING),
    ESAPI_ESCHTML("esapi_eschtml", DomainKind.STRING),
    ESAPI_ESCHTMLATTR("esapi_eschtmlattr", DomainKind.STRING),
    ESAPI_ESCXML("esapi_escxml", DomainKind.STRING),
    ESAPI_ESCXMLATTR("esapi_escxmlattr", DomainKind.STRING),
    ESAPI_ESCXPATH("esapi_escxpath", DomainKind.STRING),
    ESAPI_ESCSQL("esapi_escsql", DomainKind.STRING),

    EMTPY("emtpy", DomainKind.BOOLEAN),

    // special search procedure
    SEARCH("search", DomainKind.BOOLEAN),

    DIV("div", DomainKind.NUMERIC_Z);


    private final String sval;
    private final boolean changeable;
    private DomainKind dkind;

    NodeKind(String sval, DomainKind type) {
        this(sval, type, false);
    }

    NodeKind(String sval, DomainKind type, boolean changeable) {
        this.sval = sval;
        this.dkind = type;
        this.changeable = changeable;
    }

    public void setDomainKind(DomainKind kind) throws IllegalDomainException {
        if(!changeable)
            throw new IllegalDomainException("domain is not supposed to be " +
                    "changed for " + this.toString());

        dkind = kind;
    }


    public String toString() {
        return this.sval;
    }

    public static NodeKind KindFromString(String kind) {
        switch(kind) {
            // operations
            case "==": return EQUALS;
            case "!=": return NEQUALS;

            case "<": return SMALLER;
            case ">": return GREATER;
            case "<=": return SMALLEREQ;
            case ">=": return GREATEREQ;

            case "~~": return STR_EQUALSIC;
            case "!~": return STR_NEQUALSIC;

            case "matches": return MATCHES;
            case "startswith": return STARTSWITH;
            case "endswith": return ENDSWITH;
            case "contains": return CONTAINS;

            case "or" : return OR;
            case "xor": return XOR;
            case "and": return AND;
            case "not": return NOT;
            case "ite": return ITE;
            case "implies": return IMPLIES;

            case "substr": return SUBSTR;
            case "indexof": return INDEXOF;

            case "tolower": return TOLOWER;
            case "toupper": return TOUPPER;

            case "concat": return CONCAT;
            case "trim": return TRIM;
            case "len": return LEN;
            case "replace": return REPLACE;
            case "strinv": return STRINV;
            case "charat": return CHARAT;

            case "valueof": return VALUEOF;
            case "tostr": return TOSTR;
            case "add": return ADD;
            case "sub": return SUB;
            case "external": return EXTERNAL;

            case "apache_eschtml": return APACHE_ESCHTML;
            case "apache_ueschtml": return APACHE_UESCHTML;
            case "apache_escxml10": return APACHE_ESCXML10;
            case "apache_escxml11": return APACHE_ESCXML11;
            case "apache_escjson": return APACHE_ESCJSON;
            case "apache_escecma": return APACHE_ESCECMA;

            case "esapi_escldap": return ESAPI_ESCLDAP;
            case "esapi_escdn": return ESAPI_ESCDN;
            case "esapi_eschtml": return ESAPI_ESCHTML;
            case "esapi_eschtmlattr": return ESAPI_ESCHTMLATTR;
            case "esapi_escxml": return ESAPI_ESCXML;
            case "esapi_escxmlattr": return ESAPI_ESCXMLATTR;
            case "esapi_escxpath": return ESAPI_ESCXPATH;
            case "esapi_escsql": return ESAPI_ESCSQL;

            case "emtpy": return EMTPY;

            // meta operation
            case "search" : return SEARCH;


            // operands
            case "strlit": return STRLIT;
            case "strvar": return STRVAR;
            case "strexp": return STRREXP;

            case "numlit": return NUMLIT;
            case "numvar": return NUMVAR;

            case "boollit": return BOOLLIT;
            case "boolvar": return BOOLVAR;

            case "xmli": return XMLI;
            case "sqlistr": return SQLISTR;
            case "sqlinum": return SQLINUM;
            case "xpathstr": return XPATHSTR;
            case "xpathnum": return XPATHNUM;
            case "ldapi": return LDAPI;
            case "xss": return XSS;
            case "urli": return URLI;

            default:
                return UNKNOWN;
        }
    }

    public String getDesc() {
        if(this == EQUALS)
            return "eq";
        else if (this == STR_EQUALS)
            return "seq";
        else if (this == BOOL_EQUALS)
            return "beq";
        else return this.getValue();
    }


    public boolean isSanitizer() {
        return this == APACHE_ESCHTML || this == APACHE_UESCHTML ||
                this == APACHE_ESCXML10 || this == APACHE_ESCXML11 ||
                this == APACHE_ESCJSON || this == APACHE_ESCECMA ||
                this == ESAPI_ESCLDAP || this == ESAPI_ESCDN ||
                this == ESAPI_ESCHTML || this == ESAPI_ESCHTMLATTR ||
                this == ESAPI_ESCXML || this == ESAPI_ESCXMLATTR ||
                this == ESAPI_ESCXPATH || this == ESAPI_ESCSQL;
    }

    public boolean isComparative() {
        return this == EQUALS || this == NEQUALS || this == STR_NEQUALS || this == STR_EQUALS ||
                this == NUM_NEQUALS || this == NUM_EQUALS || this == BOOL_EQUALS ||
                this == BOOL_NEQUALS || this == STR_NEQUALSIC || this ==
                STR_EQUALSIC || this == EMTPY || this == MATCHES || this ==
                OR || this == AND || this == XOR || this == IMPLIES || this
                == NOT || this == CONTAINS || this == GREATEREQ || this ==
                SMALLEREQ || this == SMALLER || this == GREATER || this ==
                STARTSWITH || this == ENDSWITH || this == ITE;
    }

    public boolean isEquality() {
        return this == EQUALS || this == STR_EQUALS || this == NUM_EQUALS ||
                this == BOOL_EQUALS;
    }

    public boolean isInequality() {
        return this == NEQUALS || this == STR_NEQUALS || this == NUM_NEQUALS ||
                this == BOOL_NEQUALS;
    }

    public boolean isOperation() {
        return isComparative() || isSanitizer() || this == SUBSTR || this == INDEXOF || this == TOLOWER || this
                == TOUPPER || this == CONCAT || this == TRIM|| this == LEN ||
                this == REPLACE || this == STRINV || this == CHARAT || this
                == VALUEOF || this == TOSTR || this == ADD || this == SUB ||
                this == EXTERNAL;
    }

    public boolean isBranch() {
        return this == ITE;
    }

    public boolean isLiteral() {
        return this == NUMLIT || this == STRLIT || this == BOOLLIT ;
    }

    public boolean isNumeric() {
        return this.dkind.isNumeric();
    }

    public boolean isBoolean() { return this.dkind.isBoolean(); }

    public boolean isString() {
        return this.dkind.isString();
    }

    public boolean isThreatModel() {
        return this == XMLI || this == SQLISTR || this == SQLINUM ||
                this == XPATHSTR || this == XPATHNUM || this == LDAPI || this == XSS;
    }

    public boolean isRegex() {
        return this == STRREXP;
    }

    public boolean isVariable() { return !isLiteral(); }

    public DomainKind getDomainKind() {
        return this.dkind;
    }

    public String getValue() {
        return this.sval;
    }

}
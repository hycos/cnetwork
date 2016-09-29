package org.snt.cnetwork.core;

public enum OperandKind implements NetworkEntity.NetworkEntityKind {

    UNKNOWN(1,"unkown"),
    NUMVAR(3,"numvar"),
    STRVAR(5,"strvar"),
    STRREXP(7,"strexp"),
    NUMLIT(9,"numlit"),
    STRLIT(11,"strlit"),
    BOOLLIT(13,"boollit"),
    BOOLVAR(15,"boolvar"),

    XMLI(17,"xmli"),
    SQLISTR(19,"sqlistr"),
    SQLINUM(21,"sqlinum"),
    XPATHSTR(23,"xpathstr"),
    XPATHNUM(25,"xpathnum"),
    LDAPI(27,"ldapi"),
    XSS(29,"xss"),
    URLI(31,"urli");


    private static int operandId = 0;
    private final String sval;
    private final int ival;

    OperandKind(int ival, String sval) {
        this.sval = sval;
        this.ival = ival;
    }


    public int getId() {
        return this.ival;
    }

    public String toString() {
        return this.sval;
    }

    public static OperandKind KindFromString(String kind) {
        switch(kind) {

            case "unknown": return UNKNOWN;

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
        }
        // should never ever happen
        assert(false);
        return null;
    }

    public String getValue() {
        return this.sval;
    }

    public boolean isLiteral() {
        return this == NUMLIT || this == STRLIT || this == BOOLLIT ;
    }
    public boolean isNumeric() {
        return this == NUMLIT || this == NUMVAR || this == SQLINUM;
    }

    public boolean isBoolean() { return this == BOOLLIT || this == BOOLVAR; }

    public boolean isString() {
        return this == STRLIT || this == STRVAR || this == SQLISTR ||
                this == XPATHSTR || this == XSS || this == LDAPI || this == STRREXP;
    }

    public boolean isThreatModel() {
        return this == XMLI || this == SQLISTR || this == SQLINUM ||
                this == XPATHSTR || this == XPATHNUM || this == LDAPI || this == XSS;
    }

    public boolean isRegex() {
        return this == STRREXP;
    }

}


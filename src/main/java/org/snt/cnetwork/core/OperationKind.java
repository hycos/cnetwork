package org.snt.cnetwork.core;


public enum OperationKind implements NetworkEntity.NetworkEntityKind {

    UNKNOWN(0,"unknown", OperationReturnType.STRING),

    EXTERNAL(2,"external",OperationReturnType.STRING),

    // overloaded operators
    EQUALS(4,"==",OperationReturnType.BOOLEAN),
    STR_EQUALS(6,"==", OperationReturnType.BOOLEAN),
    NUM_EQUALS(8,"==",OperationReturnType.BOOLEAN),
    BOOL_EQUALS(10,"==",OperationReturnType.BOOLEAN),
    STR_EQUALSIC(12,"~~", OperationReturnType.BOOLEAN),

    NEQUALS(14,"!=", OperationReturnType.BOOLEAN),
    STR_NEQUALS(16,"!=", OperationReturnType.BOOLEAN),
    NUM_NEQUALS(18,"!=",OperationReturnType.BOOLEAN),
    BOOL_NEQUALS(20,"!=",OperationReturnType.BOOLEAN),
    STR_NEQUALSIC(22,"!~", OperationReturnType.BOOLEAN),


    SMALLER(24,"<", OperationReturnType.BOOLEAN),
    GREATER(26,">", OperationReturnType.BOOLEAN),
    SMALLEREQ(28,"<=", OperationReturnType.BOOLEAN),
    GREATEREQ(30,">=", OperationReturnType.BOOLEAN),
    MATCHES(32,"matches", OperationReturnType.BOOLEAN),
    STARTSWITH(34,"startswith", OperationReturnType.BOOLEAN),
    ENDSWITH(36,"endswith", OperationReturnType.BOOLEAN),
    CONTAINS(38,"contains", OperationReturnType.BOOLEAN),

    ASSIGNMENT(40,"=", OperationReturnType.BOOLEAN),

    OR(42,"or", OperationReturnType.BOOLEAN),
    AND(44,"and", OperationReturnType.BOOLEAN),
    XOR(46,"xor", OperationReturnType.BOOLEAN),
    NOT(48,"not", OperationReturnType.BOOLEAN),
    ITE(50,"ite", OperationReturnType.UNKNOWN),

    SUBSTR(52,"substr",OperationReturnType.STRING),
    INDEXOF(54,"indexof",OperationReturnType.NUMERIC_N),
    TOLOWER(56,"tolower",OperationReturnType.STRING_LOWER),
    TOUPPER(58,"toupper",OperationReturnType.STRING_UPPER),
    CONCAT(60,"concat",OperationReturnType.STRING),
    TRIM(62,"trim",OperationReturnType.STRING),
    LEN(64,"len",OperationReturnType.NUMERIC_N),
    REPLACE(66,"replace",OperationReturnType.STRING),
    STRINV(68,"strinv",OperationReturnType.STRING),
    CHARAT(70,"charat", OperationReturnType.STRING),

    VALUEOF(72,"valueof",OperationReturnType.NUMERIC_Z),
    TOSTR(74,"tostr",OperationReturnType.STRING),

    ADD(76,"add", OperationReturnType.NUMERIC_LZ),
    SUB(78,"sub", OperationReturnType.NUMERIC_Z),

    APACHE_ESCHTML(80,"apache_eschtml", OperationReturnType.STRING),
    APACHE_UESCHTML(82,"apache_ueschtml", OperationReturnType.STRING),
    APACHE_ESCXML10(84,"apache_escxml10", OperationReturnType.STRING),
    APACHE_ESCXML11(86,"apache_escxml11", OperationReturnType.STRING),
    APACHE_ESCJSON(88,"apache_escjson", OperationReturnType.STRING),
    APACHE_ESCECMA(90,"apache_escecma", OperationReturnType.STRING),

    ESAPI_ESCLDAP(92,"esapi_escldap", OperationReturnType.STRING),
    ESAPI_ESCDN(94,"esapi_escdn", OperationReturnType.STRING),
    ESAPI_ESCHTML(96,"esapi_eschtml", OperationReturnType.STRING),
    ESAPI_ESCHTMLATTR(98,"esapi_eschtmlattr", OperationReturnType.STRING),
    ESAPI_ESCXML(100,"esapi_escxml", OperationReturnType.STRING),
    ESAPI_ESCXMLATTR(102,"esapi_escxmlattr", OperationReturnType.STRING),
    ESAPI_ESCXPATH(104,"esapi_escxpath", OperationReturnType.STRING),
    ESAPI_ESCSQL(106,"esapi_escsql", OperationReturnType.STRING),

    EMTPY(108,"emtpy", OperationReturnType.BOOLEAN),

    // special search procedure
    SEARCH(110,"search", OperationReturnType.BOOLEAN);



    private final String sval;
    private final int ival;
    private OperationReturnType ret;
    private static int operationId = 0;

    OperationKind(int ival, String sval, OperationReturnType type) {
        this.sval = sval;
        this.ival = ival;
        this.ret = type;
    }

    private static int nextId() {
        operationId +=2;
        return operationId;
    }

    public int getId() {
        return this.ival;
    }

    public String toString() {
        return this.sval;
    }

    public boolean returnsNumericZ() {
        return this.ret == OperationReturnType.NUMERIC_Z;
    }
    public boolean returnsNumericN() {
        return this.ret == OperationReturnType.NUMERIC_N;
    }
    public boolean returnsNumericLN() {
        return this.ret == OperationReturnType.NUMERIC_LZ;
    }
    public boolean returnsString() {
        return this.ret == OperationReturnType.STRING || returnsStringLower() || returnsStringUpper() || returnsStringTrimmed();
    }
    public boolean returnsStringLower() { return this.ret == OperationReturnType.STRING_LOWER;}
    public boolean returnsStringUpper() { return this.ret == OperationReturnType.STRING_UPPER;}
    public boolean returnsStringTrimmed() { return this.ret == OperationReturnType.STRING_TRIMMED;}


    public boolean returnsBoolean() {return this.ret == OperationReturnType.BOOLEAN;}


    public static OperationKind KindFromString(String kind) {
        switch(kind) {

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

            case "=": return ASSIGNMENT;

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

            default:
                return UNKNOWN;


        }

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
                this == BOOL_NEQUALS || this == STR_NEQUALSIC || this == STR_EQUALSIC || this == EMTPY;
    }

    public void setReturnType(OperationReturnType ret){
        this.ret = ret;
    }

    public OperationReturnType getReturnType() {
        return this.ret;
    }

    public String getValue() {
        return this.sval;
    }
}
package org.snt.cnetwork.core.domain;


public enum DomainKind {

    UNKNOWN(0,"unknown"),
    NUMERIC_Z(1,"numeric_z"),
    NUMERIC_N(2,"numeric_n"),
    NUMERIC_LZ(3,"numeric_lz"),
    STRING(4,"string"),
    STRING_UPPER(5, "string_upper"),
    STRING_LOWER(6, "string_lower"),
    STRING_TRIMMED(7, "string_trimmed"),
    BOOLEAN(8,"boolean");

    private final String sval;
    private final int ival;

    DomainKind(int ival, String sval) {
        this.sval = sval;
        this.ival = ival;
    }

    public int getId() {
        return this.ival;
    }

    public String toString() {
        return this.sval;
    }

    public static DomainKind ReturnTypeFromString(String kind) {
        switch(kind) {
            case "unknown": return UNKNOWN;
            case "numeric_z": return NUMERIC_Z;
            case "numeric_n": return NUMERIC_N;
            case "numeric_zn": return NUMERIC_LZ;
            case "string": return STRING;
            case "string_lower": return STRING_LOWER;
            case "string_upper": return STRING_UPPER;
            case "string_trimmed": return STRING_TRIMMED;
            case "boolean": return BOOLEAN;
        }
        // should never ever happen
        assert(false);
        return null;
    }

    public boolean isNumeric() {
        return this == NUMERIC_Z || this == NUMERIC_LZ || this == NUMERIC_N;
    }

    public boolean isBoolean() {
        return this == BOOLEAN;
    }

    public boolean isString() {
        return this == STRING || this == STRING_LOWER || this == STRING_UPPER ||
                this == STRING_TRIMMED;
    }

}


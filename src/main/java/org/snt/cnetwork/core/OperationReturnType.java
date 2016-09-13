package org.snt.cnetwork.core;


public enum OperationReturnType {

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

    OperationReturnType(int ival, String sval) {
        this.sval = sval;
        this.ival = ival;
    }

    public int getId() {
        return this.ival;
    }

    public String toString() {
        return this.sval;
    }

    public static OperationReturnType ReturnTypeFromString(String kind) {
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
}


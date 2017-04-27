package org.snt.cnetwork.core.graph;

public enum EdgeKind {
    UNKNOWN(0,"unknown"),
    PAR_IN(1,"parin");

    private final String value;
    private int ival;

    private EdgeKind(int ival, String val) {
        this.value = val;
        this.ival = ival;
    }

    public static EdgeKind KindFromString(String kind) {

        switch (kind) {
            case "parin":
                return PAR_IN;
            case "":
                return UNKNOWN;
        }
        // should never ever happen
        assert (false);
        return null;
    }


    public String getValue() {
        return this.value;
    }


    public int getId() {
        return ival;
    }



}


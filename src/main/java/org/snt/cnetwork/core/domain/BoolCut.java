package org.snt.cnetwork.core.domain;

public class BoolCut extends NumCut {

    private final String sval;

    public static BoolCut TRUE = new BoolCut(0,"true");
    public static BoolCut FALSE = new BoolCut(1,"false");


    public BoolCut(long ival, String sval) {
        super(ival);
        this.sval = sval;
    }

    public long getId() {
        return this.endpoint;
    }

    public String toString() {
        return this.sval;
    }

    public static BoolCut KindFromString(String kind) {
        switch(kind) {
            case "true" : return TRUE.clone();
            case "false" : return FALSE.clone();
            case "0": return TRUE.clone();
            case "1": return FALSE.clone();
        }
        assert(false);
        return null;

    }

    @Override
    public BoolCut negate() {
        if(isTrue()){
            return FALSE.clone();
        }
        return TRUE.clone();
    }


    @Override
    public BoolCut clone() {
        return new BoolCut(this.endpoint, this.sval);
    }

    public String getValue() {
        return this.sval;
    }

    public boolean isTrue(){
        return this.endpoint.equals(0L);
    }

    public boolean isFalse(){
        return !isTrue();
    }


}

package org.snt.cnetwork.core.domain.range;

public class BooleanCut extends NumCut {

    private final String sval;

    public static BooleanCut TRUE = new BooleanCut(0,"true");
    public static BooleanCut FALSE = new BooleanCut(1,"false");


    public BooleanCut(long ival, String sval) {
        super(ival);
        this.sval = sval;
    }

    public long getId() {
        return this.endpoint;
    }

    public String toString() {
        return this.sval;
    }

    public static BooleanCut KindFromString(String kind) {
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
    public BooleanCut negate() {
        if(isTrue()){
            return FALSE.clone();
        }
        return TRUE.clone();
    }


    @Override
    public BooleanCut clone() {
        return new BooleanCut(this.endpoint, this.sval);
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

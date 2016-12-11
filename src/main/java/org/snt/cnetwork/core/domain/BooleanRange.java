package org.snt.cnetwork.core.domain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BooleanRange extends AtomicNumRange {

    final static Logger LOGGER = LoggerFactory.getLogger(BooleanRange.class);

    public enum BooleanValue {

        TRUE(0L,"true"),
        FALSE(1L,"false");

        private final String sval;
        private final long id;

        BooleanValue(long ival, String sval) {
            this.sval = sval;
            this.id = ival;
        }

        public long getId() {
            return this.id;
        }

        public String toString() {
            return this.sval;
        }

        public static BooleanValue KindFromString(String kind) {
            switch(kind) {
                case "true" : return TRUE;
                case "false" : return FALSE;
                case "0": return TRUE;
                case "1": return FALSE;
            }
            assert(false);
            return null;

        }

        public String getValue() {
            return this.sval;
        }
    }


    public BooleanRange() {
        super(BooleanValue.TRUE.id, BooleanValue.FALSE.id);
    }

    public BooleanRange(long min, long max) {
        super(min,max);
    }

    public BooleanRange(BooleanRange br) {
        super(br.min,br.max);
    }

    public BooleanRange(BooleanValue val) {
        super(val.id,val.id);
    }

    public BooleanRange or(BooleanRange other) {

        long newmin = (other.min + this.min) / 2;
        long newmax = (other.max + this.max) / 2;

        return new BooleanRange(newmin, newmax);
    }

    public BooleanRange and(BooleanRange other) {

        long newmin = (other.min + this.min) > 0 ? 1 : 0;
        long newmax = (other.max + this.max) > 0 ? 1 : 0;

        return new BooleanRange(newmin, newmax);
    }

    public BooleanRange xor(BooleanRange other) {

        BooleanRange pandq = this.and(other);
        BooleanRange qorq = this.or(other);

        LOGGER.debug("1" + pandq);
        LOGGER.debug("2" + qorq);

        BooleanRange neg = pandq.negate();
        LOGGER.debug("+1" + pandq);

        return qorq.and(neg);

    }

    public BooleanRange negate() {

        BooleanRange cp = this.clone();

        LOGGER.debug("NEGATE {} {}", cp.min, cp.max);
        if(cp.min == BooleanValue.TRUE.id && cp.max == BooleanValue.FALSE.id) {
            ; // nothing to do
            return cp;
        }
        assert cp.min == cp.max;
        if(cp.min == BooleanValue.TRUE.id) {
            cp.max = BooleanValue.FALSE.id;
            cp.min = BooleanValue.FALSE.id;
        } else {
            LOGGER.debug("ELSE {} {}", cp.min, BooleanValue.TRUE.id);
            cp.max = BooleanValue.TRUE.id;
            cp.min = BooleanValue.TRUE.id;
        }
        return cp;
    }

    public boolean isAlwaysTrue() {
        return (this.max == BooleanValue.TRUE.id && this.min == this.max);
    }

    public boolean isAlwaysFalse() {
        return (this.max == BooleanValue.FALSE.id && this.min == this.max);
    }

    public boolean isCatState() {
        return (this.min == BooleanValue.TRUE.id && this.max == BooleanValue.FALSE.id);
    }

    @Override
    public BooleanRange intersect(Range dother) {

        BooleanRange other = (BooleanRange)dother;

        AtomicNumRange nr = super.intersect(other);

        if(nr == null) {
            return null;
        }

        BooleanRange br = new BooleanRange(nr.getMin(), nr.getMax());

        return br;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");

        if(this.min == BooleanValue.TRUE.id) {
            sb.append("T");
        }

        if(this.max == BooleanValue.FALSE.id) {

            if(sb.length() > 1) {
                sb.append("|");
            }

            sb.append("F");
        }
        sb.append("]");
        return sb.toString();
    }

    @Override
    public BooleanRange clone() {
        return new BooleanRange(this);
    }

}

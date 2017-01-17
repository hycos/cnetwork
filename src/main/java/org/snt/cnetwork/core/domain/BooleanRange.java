package org.snt.cnetwork.core.domain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BooleanRange extends AtomicNumRange {

    final static Logger LOGGER = LoggerFactory.getLogger(BooleanRange.class);

    public BooleanRange() {
        super(BoolCut.TRUE, BoolCut.FALSE);
    }

    public BooleanRange(NumCut min, NumCut max) {
        super(min,max);
    }

    public BooleanRange(BooleanRange br) {
        super(br.getMin(),br.getMax());
    }

    public BooleanRange(BoolCut val) {
        super(val,val);
    }

    public BooleanRange or(BooleanRange other) {

        NumCut newmin = (other.getMin().add(getMin())).div(2L);
        NumCut newmax = (other.getMax().add(getMax())).diff(2L) ;

        return new BooleanRange(newmin, newmax);
    }

    public BooleanRange and(BooleanRange other) {

        NumCut newmin = (other.getMin().add(getMin()).isGreaterThan(0L)) ?
                BoolCut.FALSE.clone() : BoolCut.TRUE.clone();
        NumCut newmax = (other.getMax().add(getMax()).isGreaterThan(0L)) ?
                BoolCut.FALSE.clone() : BoolCut.TRUE.clone();

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

        LOGGER.debug("NEGATE {} {}", cp.getMin(), cp.getMax());
        if(cp.isCatState()) {
            return cp;
        }

        if(cp.isAlwaysTrue()) {
            cp.setMax(BoolCut.FALSE.clone());
            cp.setMin(BoolCut.FALSE.clone());
        } else {
            cp.setMax(BoolCut.TRUE.clone());
            cp.setMin(BoolCut.TRUE.clone());
        }
        return cp;
    }

    public boolean isAlwaysTrue() {
        return (getMax().equals(BoolCut.TRUE) && getMin() == getMax());
    }

    public boolean isAlwaysFalse() {
        return (getMax().equals(BoolCut.FALSE) && getMin() == getMax());
    }

    public boolean isCatState() {
        return (getMin().equals(BoolCut.TRUE) && getMax().equals(BoolCut
                .FALSE));
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

        if(getMin().equals(BoolCut.TRUE)) {
            sb.append("T");
        }

        if(getMax().equals(BoolCut.FALSE)) {

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

    @Override
    public boolean equals(Object o) {
        if(!(o instanceof BooleanRange))
            return false;

        BooleanRange ro = (BooleanRange)o;

        if(isAlwaysTrue() && ro.isAlwaysTrue())
            return true;

        if(isAlwaysFalse() && ro.isAlwaysFalse())
            return true;

        if(isCatState() && ro.isCatState())
            return true;

        return false;
    }

}

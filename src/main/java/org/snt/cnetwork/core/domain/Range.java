package org.snt.cnetwork.core.domain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Range implements DomainInterface<Range> {

    private final static Logger LOGGER = LoggerFactory.getLogger(Range.class);

    protected NumCut lb;
    protected NumCut ub;

    public Range(NumCut min, NumCut max) {

        LOGGER.debug("new ran min:{} max:{}", min, max);
        assert min.isSmallerEqualsThan(max);
        this.lb = min;
        this.ub = max;
    }

    public Range() {
        lb = new BelowAll();
        ub = new AboveAll();
    }

    public NumCut getMin() {
        return lb;
    }

    public void setMin(NumCut min) {
        lb = min;
    }

    public void setMax(NumCut max) {
        ub = max;
    }


    public void setMin(long min) {
        lb = new NumCut(min);
    }

    public NumCut getMax() {
        return ub;
    }

    public void setMax(long max) {
        NumCut mx = new NumCut(max);
        this.lb.isSmallerEqualsThan(mx);
        this.ub = mx;
    }

    public abstract boolean contains( long value );

    public NumCut getDiff(){

        if(ub.isFixed() && lb.isFixed())
            new NumCut(ub.sub(lb));

        if(!ub.isFixed() || !lb.isFixed())
            return new AboveAll();

        assert false;
        return null;
    }

    public boolean isAlwaysGreaterThan(Range other){
        LOGGER.debug("lb {} > other.ub {}", lb, other.ub);

        return lb.isGreaterThan(other.ub);
    }

    public boolean isAlwaysSmallerThan(Range other){
        return ub.isSmallerThan(other.lb);
    }

    public boolean isBetween(long min, long max) {
        return lb.isGreaterEqualsThan(new NumCut(min)) &&
                ub.isSmallerEqualsThan(new NumCut(max));
    }

    public boolean isBetween(NumCut min, NumCut max) {
        return lb.isGreaterEqualsThan(min) &&
                ub.isSmallerEqualsThan(max);
    }

    @Override
    public abstract Range clone();

    @Override
    public String getDomainName() {
        return "range";
    }

    @Override
    public abstract boolean equals(Object o);

}

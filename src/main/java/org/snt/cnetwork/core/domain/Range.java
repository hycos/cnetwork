package org.snt.cnetwork.core.domain;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Range implements DomainInterface<Range> {

    private final static Logger LOGGER = LoggerFactory.getLogger(Range.class);

    protected long min = 0;
    protected long max = -1;

    public Range(long min, long max) {
        assert max >= min;
        this.min = min;
        this.max = max;
    }

    public Range() {
        min = 0;
        max = 0;
    }

    public long getMin() {
        return min;
    }

    public void setMin(long min) {
        assert(min <= this.max);
        this.min = min;
    }

    public long getMax() {
        return max;
    }

    public void setMax(long max) {
        assert(max >= this.min);
        this.max = max;
    }

    public abstract boolean contains( long value );

    public long getDiff(){
        return this.max - this.min;
    }

    public boolean isAlwaysGreaterThan(Range other){
        return this.min > other.max;
    }

    public boolean isAlwaysSmallerThan(Range other){
        return this.max < other.min;
    }

    public boolean isBetween(int min, int max) {
        return this.min >= min && this.max <= max;
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

package org.snt.cnetwork.core;


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

    @Override
    public abstract Range clone();

}

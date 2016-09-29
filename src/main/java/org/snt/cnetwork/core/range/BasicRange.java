package org.snt.cnetwork.core.range;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Set;


public class BasicRange implements Comparable<BasicRange> {

    private final static Logger LOGGER = LoggerFactory.getLogger(BasicRange.class);

    public static BasicRange N = new BasicRange(0, Integer.MAX_VALUE);
    public static BasicRange Z = new BasicRange(Integer.MIN_VALUE, Integer.MAX_VALUE);

    protected long min;
    protected long max;

    public BasicRange(long min, long max) {
        assert(max >= min);
        this.min = min;
        this.max = max;
    }

    public BasicRange(long minmax) {
        this(minmax,minmax);
    }

    public BasicRange() {
        this(0,0);
    }

    public BasicRange(BasicRange nr) {
        this(nr.min,nr.max);
    }

    public boolean isAlwaysGreaterThan(BasicRange other) {
        return this.min > other.max;
    }

    public boolean isAlwaysSmallerThan(BasicRange other) {
        return this.max < other.min;
    }


    @Override
    public int compareTo(BasicRange o) {
        if(this.min < o.min)
            return -1;

        if(this.min > o.min)
            return 1;

        return 0;
    }

    @Override
    public int hashCode() {
        return (int)this.min;
    }

    @Override
    public String toString() {
        return "[" + this.min + "," + this.max + "]";
    }

    public boolean contains( long value ) {
        return value >= this.min && value <= this.max;
    }

    public boolean isBetween(int min, int max) {
        return this.min >= min && this.max <= max;
    }

    public boolean subsumes(BasicRange o) {
        return this.min <= o.getMin() && this.max >= o.getMax();
    }

    public boolean isSingleton(){
        return this.min == this.max;
    }

    public BasicRange union(BasicRange o) {

        if(this.intersection(o) == null) {
            if(this.max + 1 == o.getMin() || this.min - 1 == o.getMax()) {
                return new BasicRange(Math.min(this.max, o.getMin()), Math.max(this.max, o.max));
            } else {
                NumRange sn = new NumRange();
                sn.add(this);
                sn.add(o);
            }
        } else {
            return new BasicRange(Math.min(this.max, o.getMin()), Math.max(this.max, o.max));
        }

        return null;
    }

    public Set<BasicRange> minus(BasicRange o) {

        Set<BasicRange> ret = new HashSet<BasicRange>();
        // [3,5] - [1,3] = [4,5]
        // [1,3] - [3,5] = [1,2]
        // overlap = [3,3]

        // [1,5] - [2,2] = [1,1], [3,5]
        // [2,2] - [1,5] = empty
        // overlap = [2,2]

        // [1,10] - [5,15] = [1,4]
        // [5,15] - [1,10] = [11,15]
        // overlap = [5,10]

        // [1,3] - [7,10] = emtpy

        BasicRange overlap = this.intersection(o);

        if(this.equals(overlap) || overlap == null)
            return null;


        if(overlap.getMin() > this.getMin() &&
                overlap.getMax() < this.getMax()) {

            ret.add(new BasicRange(this.getMin(), overlap.getMin()-1));
            ret.add(new BasicRange(overlap.getMax()+1, this.getMax()));
        } else if (this.getMin() < overlap.getMin()) {
            // right
            ret.add(new BasicRange(this.getMin(), overlap.getMin()-1));
        } else if (this.getMax() > overlap.getMax()) {
            // left
            ret.add(new BasicRange(overlap.getMax()+1, this.getMax()));
        }

        return ret;
    }

    public long getDiff() {
        return this.max - this.min;
    }

    public BasicRange intersection(BasicRange other) {

        //LOGGER.info("get overlap " + this.toString() + " " + other.toString());

        if(other.min > this.max || other.max < this.min)
            return null;

        BasicRange overlap = new BasicRange(Math.max(this.min, other.min),Math.min(this.max, other.max));
        //LOGGER.info("Overlap is "+ overlap);

        assert(overlap.max >= overlap.min);

        return overlap;
    }


    public boolean equals(int min, int max) {
        return this.equals(new BasicRange(min,max));
    }


    @Override
    public boolean equals(Object o) {
        if(!(o instanceof BasicRange))
            return false;

        BasicRange r = (BasicRange)o;
        return (r.getMin() == this.min && r.getMax() == this.max);
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

    public BasicRange numadd(BasicRange other) {
        return new BasicRange(Math.min(other.min + this.min, other.max + this.max),
                Math.max(other.min + this.min, other.max + this.max));
    }

    public BasicRange numsub(BasicRange other) {
        return new BasicRange(Math.min(this.min - other.min, this.max - other.max),
                Math.max(this.min - other.min, this.max - other.max));
    }

    public BasicRange getPosSubRange(){
        return this.intersection(N);
    }

    public BasicRange getNegSubRange() {
        BasicRange zran = Z.clone();
        zran.setMax(-1);
        return this.intersection(zran);
    }

    public Automaton getLenAutomaton() {

        BasicRange isect = this.intersection(N.clone());

        LOGGER.info("getlenauto" + isect.toString());


        if(isect == null) {
            return new RegExp(".{0}").toAutomaton();
        }

        if(isect.getMax() == N.getMax()) {
            return new RegExp(".*").toAutomaton();
        } else {
            return new RegExp(".{" + this.getMin() + "," + this.getMax() + "}").toAutomaton();
        }
    }

    @Override
    public BasicRange clone() {
        return new BasicRange(this);
    }


}

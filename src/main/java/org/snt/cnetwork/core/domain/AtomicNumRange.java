package org.snt.cnetwork.core.domain;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class AtomicNumRange extends Range {

    private final static Logger LOGGER = LoggerFactory.getLogger(AtomicNumRange.class);

    public static AtomicNumRange N = new AtomicNumRange(0, Integer.MAX_VALUE);
    public static AtomicNumRange Z = new AtomicNumRange(Integer.MIN_VALUE, Integer.MAX_VALUE);


    public AtomicNumRange(long min, long max) {
        super(min,max);
    }

    public AtomicNumRange(long minmax) {
        this(minmax,minmax);
    }

    public AtomicNumRange() {
        super(0,0);
    }

    public AtomicNumRange(AtomicNumRange nr) {
        this(nr.min,nr.max);
    }

    public boolean isAlwaysGreaterThan(AtomicNumRange other) {
        return this.min > other.max;
    }

    public boolean isAlwaysSmallerThan(AtomicNumRange other) {
        return this.max < other.min;
    }


    @Override
    public boolean isEmpty() {
        return max < min;
    }

    @Override
    public int hashCode() {
        return (int)this.min;
    }

    @Override
    public String toString() {
        return "[" + this.min + "," + this.max + "]";
    }

    @Override
    public boolean contains( long value ) {
        return value >= this.min && value <= this.max;
    }

    public boolean isBetween(int min, int max) {
        return this.min >= min && this.max <= max;
    }

    @Override
    public boolean subsumes(Range dother) {

        assert dother instanceof AtomicNumRange;

        AtomicNumRange o = (AtomicNumRange)dother;

        return this.min <= o.getMin() && this.max >= o.getMax();
    }

    @Override
    public boolean isSingleton(){
        return this.min == this.max;
    }

    @Override
    public AtomicNumRange union(Range dother) {

        assert dother instanceof AtomicNumRange;

        AtomicNumRange o = (AtomicNumRange)dother;

        if(intersect(o) == null) {
            if(this.max + 1 == o.getMin() || this.min - 1 == o.getMax()) {
                return new AtomicNumRange(Math.min(this.max, o.getMin()), Math.max(this.max, o.max));
            } else {
                NumRange sn = new NumRange();
                sn.add(this);
                sn.add(o);
            }
        } else {
            return new AtomicNumRange(Math.min(this.max, o.getMin()), Math.max(this.max, o.max));
        }

        return null;
    }

    @Override
    public NumRange complement(Range dother) {
        assert dother instanceof AtomicNumRange;
        AtomicNumRange o = (AtomicNumRange)dother;
        return this.minus(o);
    }


    @Override
    public NumRange minus(Range dother) {
        assert dother instanceof AtomicNumRange;

        AtomicNumRange o = (AtomicNumRange)dother;

        NumRange nr = new NumRange();
        //Set<AtomicNumRange> ret = new HashSet<AtomicNumRange>();
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

        AtomicNumRange overlap = intersect(o);

        if(this.equals(overlap) || overlap == null)
            return null;


        if(overlap.getMin() > this.getMin() &&
                overlap.getMax() < this.getMax()) {

            nr.add(new AtomicNumRange(this.getMin(), overlap.getMin()-1));
            nr.add(new AtomicNumRange(overlap.getMax()+1, this.getMax()));
        } else if (this.getMin() < overlap.getMin()) {
            // right
            nr.add(new AtomicNumRange(this.getMin(), overlap.getMin()-1));
        } else if (this.getMax() > overlap.getMax()) {
            // left
            nr.add(new AtomicNumRange(overlap.getMax()+1, this.getMax()));
        }
        return nr;
    }

    public long getDiff() {
        return this.max - this.min;
    }

    @Override
    public AtomicNumRange intersect(Range dother) {

        assert dother instanceof AtomicNumRange;

        AtomicNumRange other = (AtomicNumRange)dother;

        LOGGER.info("get overlap " + this.toString() + " " + other.toString
                 ());

        if(other.min > this.max || other.max < this.min)
            return null;

        AtomicNumRange overlap = new AtomicNumRange(Math.max(this.min, other.min),Math.min(this.max, other.max));
        //LOGGER.info("Overlap is "+ overlap);

        assert(overlap.max >= overlap.min);

        return overlap;
    }


    public boolean equals(int min, int max) {
        return this.equals(new AtomicNumRange(min,max));
    }


    @Override
    public boolean equals(Object o) {
        if(!(o instanceof AtomicNumRange))
            return false;

        AtomicNumRange r = (AtomicNumRange)o;
        return (r.getMin() == this.min && r.getMax() == this.max);
    }


    public AtomicNumRange numadd(AtomicNumRange other) {
        return new AtomicNumRange(Math.min(other.min + this.min, other.max + this.max),
                Math.max(other.min + this.min, other.max + this.max));
    }

    public AtomicNumRange numsub(AtomicNumRange other) {
        return new AtomicNumRange(Math.min(this.min - other.min, this.max - other.max),
                Math.max(this.min - other.min, this.max - other.max));
    }

    public AtomicNumRange getPosSubRange(){
        return intersect(N);
    }

    public AtomicNumRange getNegSubRange() {
        AtomicNumRange zran = Z.clone();
        zran.setMax(-1);
        return intersect(zran);
    }

    public Automaton getLenAutomaton() {

        AtomicNumRange isect = intersect(N.clone());

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
    public AtomicNumRange clone() {
        return new AtomicNumRange(this);
    }


}

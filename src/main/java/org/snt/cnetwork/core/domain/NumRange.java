package org.snt.cnetwork.core.domain;

import dk.brics.automaton.Automaton;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class NumRange extends Range {

    private final static Logger LOGGER = LoggerFactory.getLogger(NumRange.class);

    private TreeMap<Long, AtomicNumRange> ran;

    public NumRange() {
        this.ran = new TreeMap();
    }


    public NumRange(long c) {
        this();
        this.add(c);
    }

    public NumRange(AtomicNumRange ar) {
        this.ran = new TreeMap();
        add(ar);
    }

    public NumRange(NumRange nr) {
        this();
        for(Map.Entry<Long, AtomicNumRange> e : nr.ran.entrySet()) {
            this.ran.put(e.getKey(), e.getValue().clone());
        }
        this.min = nr.min;
        this.max = nr.max;
    }



    @Override
    public NumRange minus(Range dother) {

        assert dother instanceof NumRange;

        NumRange other = (NumRange)dother;
        // all the elements that are not contained in this
        NumRange complement = other.complement(this);
        //LOGGER.info("COMPLEMEN " + complement);

        if(complement == null)
            return null;

        return complement.intersect(this);
    }


    @Override
    public NumRange complement(Range dother) {

        assert dother instanceof NumRange;

        NumRange other = (NumRange)dother;

        NumRange ret = null;

        Map.Entry<Long, AtomicNumRange> next = null;

        for(Map.Entry<Long, AtomicNumRange> e : this.ran.entrySet()) {
            next = this.ran.higherEntry(e.getKey());

           if(next != null) {
               //LOGGER.info("NEXT " + next.toString());
               if(ret == null) ret = new NumRange();
               ret.add(new AtomicNumRange(e.getValue().getMax() + 1, next.getValue().getMin()-1));
            }
        }

        if(other.min < this.min) {
            if(ret == null) ret = new NumRange();
            ret.add(new AtomicNumRange(other.min, this.min - 1));
        }
        if(other.max > this.max) {
            if(ret == null) ret = new NumRange();
            ret.add(new AtomicNumRange(this.max + 1, other.max));
        }

        return ret;

    }

    @Override
    public boolean subsumes(Range dother) {
        assert dother instanceof NumRange;

        NumRange other = (NumRange)dother;

        NumRange n = this.intersect(other);

        return n.equals(dother);
    }

    @Override
    public boolean contains(long val) {
        NumRange nr = intersect(new NumRange(val));
        return nr != null;
    }

    @Override
    public boolean isSingleton() {
        return this.getRangeMap().size() == 1 &&
                this.getRangeMap().get(0).isSingleton();
    }

    @Override
    public boolean isEmpty() {
        return this.getRangeMap().isEmpty();
    }

    @Override
    public NumRange intersect(Range dother) {

        assert dother instanceof NumRange;

        NumRange other = (NumRange)dother;

        //LOGGER.info("Get intersection: " + this.toString() + " " + nr.toString());

        NumRange rs = null;

        // entries to consider
        Map.Entry <Long, AtomicNumRange> thisfrom = (this.ran.floorEntry(other.min) == null ?
                this.ran.ceilingEntry(other.min) : this.ran.floorEntry(other.min));
        Map.Entry <Long, AtomicNumRange> thisto = (this.ran.ceilingEntry(other.max) == null ?
                this.ran.floorEntry(other.max) : this.ran.ceilingEntry(other.max));


        assert(thisfrom != null);
        assert(thisto != null);

        if(thisfrom == null || thisto == null)
            return rs;

        for(Map.Entry <Long, AtomicNumRange> thisptr = thisfrom;
            thisptr != null;
            thisptr = this.ran.higherEntry(thisptr.getValue().getMin())) {

            //LOGGER.info(">> " + thisptr.getValue().toString());
            long thismin = thisptr.getValue().getMin();
            long thismax = thisptr.getValue().getMax();

            Map.Entry <Long, AtomicNumRange> sfrom = (other.ran.floorEntry(thismin) == null ?
                    other.ran.ceilingEntry(thismin) : other.ran.floorEntry(thismin));
            Map.Entry <Long, AtomicNumRange> sto = (other.ran.ceilingEntry(thismax) == null ?
                    other.ran.floorEntry(thismax) : other.ran.ceilingEntry(thismax));


            //LOGGER.info("SFROM " + sfrom.getValue().toString());
            //LOGGER.info("STO " + sto.getValue().toString());

            if(sfrom == null || sto == null)
                continue;


            for(Map.Entry <Long, AtomicNumRange> sptr = sfrom;
                sptr != null;
                sptr = other.ran.higherEntry(sptr.getKey())) {
                //LOGGER.info("++==");

                Set<AtomicNumRange> toadd = new HashSet<AtomicNumRange>();

                AtomicNumRange ret = sptr.getValue().intersect(thisptr.getValue());

                //LOGGER.info("*** " + sptr);

                if(ret != null)
                    toadd.add(ret);

                if(toadd != null) {
                    if(rs == null) {
                       rs = new NumRange();
                    }
                    rs.addAll(toadd);
                }

                if(sptr == sto)
                    break;
            }


            if(thisptr == thisto)
                break;
        }
        //LOGGER.info("DONE ");

        return rs;
    }


    @Override
    public NumRange union(Range dother) {
        assert dother instanceof NumRange;
        NumRange other = (NumRange)dother;
        NumRange nr = new NumRange(this);
        nr.addAll(other.getRangeMap().values());
        return nr;
    }


    private void addAll(Collection<AtomicNumRange> s) {
        for (AtomicNumRange nr : s) {
            add(nr);
        }
    }

    public void add(long constant) {
        add(new AtomicNumRange(constant,constant));
    }

    public void add(long min, long max) {
        this.add(new AtomicNumRange(min,max));
    }

    public void add(AtomicNumRange e) {

        //LOGGER.info("ADD NR " + e);

        if(ran.isEmpty()){
            ran.put(e.getMin(), e);
            min = e.getMin();
            max = e.getMax();
            return;
        }

        // e.max >= e.min ==> florMax => ceilMin
        Map.Entry <Long, AtomicNumRange> floorMax = ran.floorEntry(e.getMax());
        // e or e's direct successor
        Map.Entry <Long, AtomicNumRange> ceilMin = ran.ceilingEntry(e.getMin());

        AtomicNumRange fMax = null;
        AtomicNumRange cMin = null;

        if(floorMax != null) {
            fMax = floorMax.getValue();
            //LOGGER.info("FMAX : " + fMax.toString());
        }

        if(ceilMin != null) {
            cMin = ceilMin.getValue();
            //LOGGER.info("CMIN : " + cMin.toString());
        }

        if(cMin != null) {
            //LOGGER.info("here");
            AtomicNumRange overlap = cMin.intersect(e);
            if(overlap != null) {
                long min = Math.min(cMin.getMin(), e.getMin());
                long max = (fMax != null ? Math.max(fMax.getMax(),e.getMax()) : e.getMax());
                AtomicNumRange newfmin = new AtomicNumRange(min,max);
                cleanupDescending(newfmin);
                this.ran.remove(cMin.getMin());
                join(newfmin);
                return;
            }
        }

        if(fMax != null) {
            //LOGGER.info("there");
            AtomicNumRange overlap = fMax.intersect(e);
            if (overlap != null) {
                long min = (ceilMin != null ? Math.min(ceilMin.getValue().getMin(), e.getMin()) : e.getMin());
                long max = Math.max(fMax.getMax(), e.getMax());
                //LOGGER.info("MIN " + min + " " + fMax.getMax());
                AtomicNumRange newfmax = new AtomicNumRange(min, max);
                cleanupAscending(newfmax);
                //this.remove(fMax.getMin());
                join(newfmax);
                return;
            }
        }

        join(e);
    }


    private void join(AtomicNumRange e) {

        // We assume (in the general case) that we have the following order rfloor:e:rceil
        Map.Entry<Long, AtomicNumRange> ceil = ran.ceilingEntry(e.getMax());
        Map.Entry<Long, AtomicNumRange> floor = ran.floorEntry(e.getMin());

        AtomicNumRange rceil = null;
        AtomicNumRange rfloor = null;
        AtomicNumRange nr = new AtomicNumRange(e);

        boolean removeFloor = false;
        boolean removeCeil = false;


        if(floor != null) {
            rfloor = floor.getValue();
            //LOGGER.info("RFLOOR " + rfloor);
            if(e.intersect(rfloor) != null || e.getMin() - 1 == rfloor.getMax()) {
                nr.setMin(rfloor.getMin());
                removeFloor = true;
            }
        }

        if(ceil != null) {
            rceil = ceil.getValue();
            //LOGGER.info("RCEIL " + rceil);
            if(e.intersect(rceil) != null || e.getMax() + 1 == rceil.getMin()) {
                nr.setMax(rceil.getMax());
                removeCeil = true;
            }
        }

        if(removeFloor)
            ran.remove(rfloor.getMin());

        if(removeCeil)
            ran.remove(rceil.getMin());

        if(nr.getMin() < min)
            min = nr.getMin();

        if(nr.getMax() > max)
            max = nr.getMax();

        ran.put(nr.getMin(), nr);

    }

    private void cleanupDescending(AtomicNumRange from) {
        LOGGER.info("cleanup desc " + from) ;
        Map.Entry<Long, AtomicNumRange> todel = ran.higherEntry(from.getMin());

        while (todel != null &&
                from.subsumes(todel.getValue())&&
                todel.getValue() != from) {

            ran.remove(todel.getKey());
            todel = ran.higherEntry(todel.getValue().getMin());
        }
    }

    private void cleanupAscending(AtomicNumRange from) {
        LOGGER.info("cleanup asc " + from);
        Map.Entry<Long, AtomicNumRange> todel = ran.lowerEntry(from.getMax());

        while (todel != null &&
                from.subsumes(todel.getValue())&&
                todel.getValue() != from) {

            ran.remove(todel.getKey());
            todel = ran.lowerEntry(todel.getValue().getMax());
        }
    }

    public int size(){
        return this.ran.size();
    }

    @Override
    public boolean equals(Object o) {

        if(!(o instanceof NumRange)){
            return false;
        }

        NumRange rs = (NumRange)o;

        if(ran.size() != rs.ran.size())
            return false;

        if(min != rs.min || max != rs.max)
            return false;

        for(Map.Entry<Long, AtomicNumRange> e : ran.entrySet()) {

            AtomicNumRange other = rs.ran.get(e.getKey());

            if(!e.getValue().equals(other))
                return false;

        }
        return true;
    }

    public TreeMap<Long, AtomicNumRange> getRangeMap() {
        return this.ran;
    }

    public Automaton getLenAutomaton() {
        Automaton automaton = null;
        for(Map.Entry<Long, AtomicNumRange> e : this.ran.entrySet()) {
            if(automaton == null) {
                automaton = e.getValue().getLenAutomaton();
            } else {
                automaton = automaton.union(e.getValue().getLenAutomaton());
            }
        }
        return automaton;
    }


    public NumRange numadd(NumRange nr) {
        NumRange ret = new NumRange();
        for(Map.Entry<Long, AtomicNumRange> eout : this.ran.entrySet()) {
            for(Map.Entry<Long, AtomicNumRange> ein : nr.ran.entrySet()) {

                AtomicNumRange sum = eout.getValue().numadd(ein.getValue());

                LOGGER.info(eout.getValue() + "+" + ein.getValue() + "=" + sum);
                ret.add(sum);
            }
        }
        return ret;
    }

    public NumRange numsub(NumRange nr) {
        NumRange ret = new NumRange();
        for(Map.Entry<Long, AtomicNumRange> eout : this.ran.entrySet()) {
            for(Map.Entry<Long, AtomicNumRange> ein : nr.ran.entrySet()) {

                AtomicNumRange sum = eout.getValue().numsub(ein.getValue());

                LOGGER.info(eout.getValue() + "-" + ein.getValue() + "=" + sum);
                ret.add(sum);
            }
        }
        return ret;
    }


    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();

        int k = 0;

        sb.append("[");
        for(Map.Entry<Long, AtomicNumRange> e : this.ran.entrySet()) {
            if(k++ > 0) {
                sb.append(",");
            }
            sb.append(e.getValue().toString());
        }
        sb.append("]{" + this.min + "," + this.max + "}" );
        return sb.toString();
    }


    @Override
    public Range clone() {
        return new NumRange(this);
    }

    @Override
    public void setMin(long min) {
        assert(min <= this.max);
        NumRange nr = new NumRange(new AtomicNumRange(min, this.max));
        NumRange isect = this.intersect(nr);
        this.ran.clear();
        this.ran.putAll(isect.ran);
    }

    @Override
    public void setMax(long max) {
        assert(min <= this.max);
        NumRange nr = new NumRange(new AtomicNumRange(this.min, max));
        NumRange isect = this.intersect(nr);
        this.ran.clear();
        this.ran.putAll(isect.ran);
    }
}

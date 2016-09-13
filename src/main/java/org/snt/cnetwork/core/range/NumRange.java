package org.snt.cnetwork.core.range;

import dk.brics.automaton.Automaton;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class NumRange extends BasicRange {

    final static Logger logger = LoggerFactory.getLogger(NumRange.class);

    public static NumRange N = new NumRange(BasicRange.N);
    public static NumRange Z = new NumRange(BasicRange.Z);

    private TreeMap<Long, BasicRange> ran;

    public NumRange() {
        this.ran = new TreeMap<Long, BasicRange>();
    }

    public NumRange(BasicRange ar) {
        this.ran = new TreeMap<Long, BasicRange>();
        add(ar);
    }

    public NumRange(NumRange nr) {
        this();
        for(Map.Entry<Long, BasicRange> e : nr.ran.entrySet()) {
            this.ran.put(e.getKey(), e.getValue().clone());
        }
        this.min = nr.min;
        this.max = nr.max;
    }



    public NumRange minus(NumRange nr) {

        // all the elements that are not contained in this
        NumRange complement = nr.complement(this);
        //logger.info("COMPLEMEN " + complement);

        if(complement == null)
            return null;

        return complement.intersection(this);
    }


    public NumRange complement(BasicRange br) {


        NumRange ret = null;

        Map.Entry<Long, BasicRange> next = null;

        for(Map.Entry<Long, BasicRange> e : this.ran.entrySet()) {

            next = this.ran.higherEntry(e.getKey());

           if(next != null) {
               //logger.info("NEXT " + next.toString());
               if(ret == null) ret = new NumRange();
               ret.add(new BasicRange(e.getValue().getMax() + 1, next.getValue().getMin()-1));
            }
        }

        if(br.min < this.min) {
            if(ret == null) ret = new NumRange();
            ret.add(new BasicRange(br.min, this.min - 1));
        }
        if(br.max > this.max) {
            if(ret == null) ret = new NumRange();
            ret.add(new BasicRange(this.max + 1, br.max));
        }

        return ret;

    }


    public NumRange intersection(NumRange nr) {

        //logger.info("Get intersection: " + this.toString() + " " + nr.toString());

        NumRange rs = null;

        // entries to consider
        Map.Entry <Long, BasicRange> thisfrom = (this.ran.floorEntry(nr.min) == null ?
                this.ran.ceilingEntry(nr.min) : this.ran.floorEntry(nr.min));
        Map.Entry <Long, BasicRange> thisto = (this.ran.ceilingEntry(nr.max) == null ?
                this.ran.floorEntry(nr.max) : this.ran.ceilingEntry(nr.max));


        assert(thisfrom != null);
        assert(thisto != null);

        if(((BasicRange)this).intersection(((BasicRange)nr)) == null) {
            return null;
        }


        if(thisfrom == null || thisto == null)
            return rs;

        for(Map.Entry <Long, BasicRange> thisptr = thisfrom;
            thisptr != null;
            thisptr = this.ran.higherEntry(thisptr.getValue().getMin())) {

            //logger.info(">> " + thisptr.getValue().toString());
            long thismin = thisptr.getValue().getMin();
            long thismax = thisptr.getValue().getMax();

            Map.Entry <Long, BasicRange> sfrom = (nr.ran.floorEntry(thismin) == null ?
                    nr.ran.ceilingEntry(thismin) : nr.ran.floorEntry(thismin));
            Map.Entry <Long, BasicRange> sto = (nr.ran.ceilingEntry(thismax) == null ?
                    nr.ran.floorEntry(thismax) : nr.ran.ceilingEntry(thismax));


            //logger.info("SFROM " + sfrom.getValue().toString());
            //logger.info("STO " + sto.getValue().toString());

            if(sfrom == null || sto == null)
                continue;


            for(Map.Entry <Long, BasicRange> sptr = sfrom;
                sptr != null;
                sptr = nr.ran.higherEntry(sptr.getKey())) {
                //logger.info("++==");



                Set<BasicRange> toadd = new HashSet<BasicRange>();

                BasicRange ret = sptr.getValue().intersection(thisptr.getValue());

                //logger.info("*** " + sptr);

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
        //logger.info("DONE ");

        return rs;
    }


    private void addAll(Collection<BasicRange> s) {

        for (BasicRange nr : s) {
            this.add(nr);
        }
    }

    public void add (int constant) {
        this.add(new BasicRange(constant,constant));
    }

    public void add (int min, int max) {
        this.add(new BasicRange(min,max));
    }

    public void add(BasicRange e) {

        //logger.info("ADD NR " + e);

        if(this.ran.isEmpty()){
            this.ran.put(e.getMin(), e);
            this.min = e.getMin();
            this.max = e.getMax();
            return;
        }

        // e.max >= e.min ==> florMax => ceilMin
        Map.Entry <Long, BasicRange> floorMax = this.ran.floorEntry(e.getMax());
        // e or e's direct successor
        Map.Entry <Long, BasicRange> ceilMin = this.ran.ceilingEntry(e.getMin());

        BasicRange fMax = null;
        BasicRange cMin = null;

        if(floorMax != null) {
            fMax = floorMax.getValue();
            //logger.info("FMAX : " + fMax.toString());
        }

        if(ceilMin != null) {
            cMin = ceilMin.getValue();
            //logger.info("CMIN : " + cMin.toString());
        }

        if(cMin != null) {
            //logger.info("here");
            BasicRange overlap = cMin.intersection(e);
            if(overlap != null) {
                long min = Math.min(cMin.getMin(), e.getMin());
                long max = (fMax != null ? Math.max(fMax.getMax(),e.getMax()) : e.getMax());
                BasicRange newfmin = new BasicRange(min,max);
                cleanupDescending(newfmin);
                this.ran.remove(cMin.getMin());
                join(newfmin);
                return;
            }
        }

        if(fMax != null) {
            //logger.info("there");
            BasicRange overlap = fMax.intersection(e);
            if (overlap != null) {
                long min = (ceilMin != null ? Math.min(ceilMin.getValue().getMin(), e.getMin()) : e.getMin());
                long max = Math.max(fMax.getMax(), e.getMax());
                //logger.info("MIN " + min + " " + fMax.getMax());
                BasicRange newfmax = new BasicRange(min, max);
                cleanupAscending(newfmax);
                //this.remove(fMax.getMin());
                join(newfmax);
                return;
            }
        }

        join(e);
    }


    private void join(BasicRange e) {

        // We assume (in the general case) that we have the following order rfloor:e:rceil
        Map.Entry<Long, BasicRange> ceil = this.ran.ceilingEntry(e.getMax());
        Map.Entry<Long, BasicRange> floor = this.ran.floorEntry(e.getMin());

        BasicRange rceil = null;
        BasicRange rfloor = null;
        BasicRange nr = new BasicRange(e);

        boolean removeFloor = false;
        boolean removeCeil = false;


        if(floor != null) {
            rfloor = floor.getValue();
            //logger.info("RFLOOR " + rfloor);
            if(e.intersection(rfloor) != null || e.getMin() - 1 == rfloor.getMax()) {
                nr.setMin(rfloor.getMin());
                removeFloor = true;
            }
        }

        if(ceil != null) {
            rceil = ceil.getValue();
            //logger.info("RCEIL " + rceil);
            if(e.intersection(rceil) != null || e.getMax() + 1 == rceil.getMin()) {
                nr.setMax(rceil.getMax());
                removeCeil = true;
            }
        }

        if(removeFloor)
            this.ran.remove(rfloor.getMin());

        if(removeCeil)
            this.ran.remove(rceil.getMin());

        if(nr.getMin() < this.min)
            this.min = nr.getMin();

        if(nr.getMax() > this.max)
            this.max = nr.getMax();

        this.ran.put(nr.getMin(), nr);

    }

    private void cleanupDescending(BasicRange from) {
        logger.info("cleanup desc " + from) ;
        Map.Entry<Long, BasicRange> todel = this.ran.higherEntry(from.getMin());

        while (todel != null &&
                from.subsumes(todel.getValue())&&
                todel.getValue() != from) {

            this.ran.remove(todel.getKey());
            todel = this.ran.higherEntry(todel.getValue().getMin());
        }
    }

    private void cleanupAscending(BasicRange from) {
        logger.info("cleanup asc " + from);
        Map.Entry<Long, BasicRange> todel = this.ran.lowerEntry(from.getMax());

        while (todel != null &&
                from.subsumes(todel.getValue())&&
                todel.getValue() != from) {

            this.ran.remove(todel.getKey());
            todel = this.ran.lowerEntry(todel.getValue().getMax());
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

        if(this.ran.size() != rs.ran.size())
            return false;

        if(this.min != rs.min || this.max != rs.max)
            return false;

        for(Map.Entry<Long, BasicRange> e : this.ran.entrySet()) {

            BasicRange other = rs.ran.get(e.getKey());

            if(!e.getValue().equals(other))
                return false;

        }
        return true;
    }

    public TreeMap<Long, BasicRange> getRangeMap() {
        return this.ran;
    }

    public Automaton getLenAutomaton() {
        Automaton automaton = null;
        for(Map.Entry<Long, BasicRange> e : this.ran.entrySet()) {
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
        for(Map.Entry<Long, BasicRange> eout : this.ran.entrySet()) {
            for(Map.Entry<Long, BasicRange> ein : nr.ran.entrySet()) {

                BasicRange sum = eout.getValue().numadd(ein.getValue());

                logger.info(eout.getValue() + "+" + ein.getValue() + "=" + sum);
                ret.add(sum);
            }
        }
        return ret;
    }

    public NumRange numsub(NumRange nr) {
        NumRange ret = new NumRange();
        for(Map.Entry<Long, BasicRange> eout : this.ran.entrySet()) {
            for(Map.Entry<Long, BasicRange> ein : nr.ran.entrySet()) {

                BasicRange sum = eout.getValue().numsub(ein.getValue());

                logger.info(eout.getValue() + "-" + ein.getValue() + "=" + sum);
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
        for(Map.Entry<Long, BasicRange> e : this.ran.entrySet()) {
            if(k++ > 0) {
                sb.append(",");
            }
            sb.append(e.getValue().toString());
        }
        sb.append("]{" + this.min + "," + this.max + "}" );
        return sb.toString();
    }


    @Override
    public BasicRange clone() {
        return new NumRange(this);
    }

    @Override
    public void setMin(long min) {
        assert(min <= this.max);

        NumRange nr = new NumRange(new BasicRange(min, this.max));
        NumRange isect = this.intersection(nr);

        this.ran.clear();
        this.ran.putAll(isect.ran);

    }

    @Override
    public void setMax(long max) {
        assert(min <= this.max);

        NumRange nr = new NumRange(new BasicRange(this.min, max));
        NumRange isect = this.intersection(nr);

        this.ran.clear();
        this.ran.putAll(isect.ran);

    }



}

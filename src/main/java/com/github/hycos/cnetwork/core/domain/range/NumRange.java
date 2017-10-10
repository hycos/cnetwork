/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetwork.core.domain.range;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.domain.automaton.SimpleAutomaton;
import com.github.hycos.cnetwork.core.graph.Node;

import java.util.*;

public class NumRange extends Range {

    private final static Logger LOGGER = LoggerFactory.getLogger(NumRange.class);

    public static NumRange N = new NumRange(new AtomicNumRange(new NumCut(0L),
            new AboveAll()));

    public static NumRange Z = new NumRange(new AtomicNumRange());

    private TreeMap<NumCut, AtomicNumRange> ran = new TreeMap<>();

    protected NumRange() {

    }


    public NumRange(long c) {
        add(c);
    }

    public NumRange(AtomicNumRange ar) {
        add(ar);
    }

    public NumRange(Collection<AtomicNumRange> ar) {
        assert !ar.isEmpty();
        addAll(ar);
    }

    public NumRange(NumRange nr) {
        for(Map.Entry<NumCut, AtomicNumRange> e : nr.ran.entrySet()) {
            this.ran.put(e.getKey().clone(), e.getValue().clone());
        }
        this.lb = nr.getMin().clone();
        this.ub = nr.getMax().clone();
    }

    @Override
    public NumRange minus(Range dother) {

        assert dother instanceof NumRange;

        NumRange other = (NumRange)dother;
        // all the elements that are not contained in this
        NumRange complement = other.complement(this);
        //LOGGER.debug("COMPLEMEN " + complement);

        if(complement == null)
            return null;

        return complement.intersect(this);
    }


    @Override
    public NumRange complement() {
        return Z.clone().complement(this);
    }

    private NumRange complement(NumRange dother) {

        assert dother instanceof NumRange;

        NumRange other = (NumRange)dother;

        NumRange ret = null;

        Map.Entry<NumCut, AtomicNumRange> next = null;

        for(Map.Entry<NumCut, AtomicNumRange> e : this.ran.entrySet()) {
            next = this.ran.higherEntry(e.getKey());

           if(next != null) {
               //LOGGER.debug("NEXT " + next.toString());
               if(ret == null) ret = new NumRange();
               ret.add(new AtomicNumRange(e.getValue().getMax().add(1L), next
                       .getValue().getMin().sub(1L)));
            }
        }

        if(other.getMin().isSmallerThan(this.getMin())) {
            if(ret == null) ret = new NumRange();
            ret.add(new AtomicNumRange(other.getMin(), getMin().sub(1L)));
        }
        if(other.getMax().isGreaterThan(this.getMax())) {
            if(ret == null) ret = new NumRange();
            ret.add(new AtomicNumRange(getMax().add(1L), other.getMax()));
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
                this.getRangeMap().firstEntry().getValue().isSingleton();
    }

    @Override
    public boolean isEmpty() {
        //LOGGER.debug("RM {}", this.getRangeMap().size());
        return this.getRangeMap().isEmpty();
    }


    @Override
    public void init(Node n) {
        // nothing to do
    }

    @Override
    public NumRange intersect(Range dother) {

        assert dother instanceof NumRange;

        NumRange other = (NumRange)dother;

        LOGGER.debug("Get intersection: " + this.toString() + " " + dother.toString
                ());

        NumRange rs = null;

        // entries to consider
        Map.Entry <NumCut, AtomicNumRange> thisfrom = (this.ran.floorEntry
                (other.getMin()) == null ?
                this.ran.ceilingEntry(other.getMin()) : this.ran.floorEntry(other
                .getMin()));
        Map.Entry <NumCut, AtomicNumRange> thisto = (this.ran.ceilingEntry
                (other.getMax()) == null ?
                this.ran.floorEntry(other.getMax()) : this.ran.ceilingEntry(other
                .getMax()));


        //assert(thisfrom != null);
        //assert(thisto != null);

        if(thisfrom == null || thisto == null)
            return rs;

        for(Map.Entry <NumCut, AtomicNumRange> thisptr = thisfrom;
            thisptr != null;
            thisptr = this.ran.higherEntry(thisptr.getValue().getMin())) {

            LOGGER.debug(">> " + thisptr.getValue().toString());
            NumCut thismin = thisptr.getValue().getMin();
            NumCut thismax = thisptr.getValue().getMax();

            Map.Entry <NumCut, AtomicNumRange> sfrom = (other.ran.floorEntry(thismin) == null ?
                    other.ran.ceilingEntry(thismin) : other.ran.floorEntry(thismin));
            Map.Entry <NumCut, AtomicNumRange> sto = (other.ran.ceilingEntry(thismax) == null ?
                    other.ran.floorEntry(thismax) : other.ran.ceilingEntry(thismax));


            //LOGGER.debug("SFROM " + sfrom.getValue().toString());
            //LOGGER.debug("STO " + sto.getValue().toString());

            if(sfrom == null || sto == null)
                continue;


            for(Map.Entry <NumCut, AtomicNumRange> sptr = sfrom;
                sptr != null;
                sptr = other.ran.higherEntry(sptr.getKey())) {
                //LOGGER.debug("++==");

                Set<AtomicNumRange> toadd = new HashSet<AtomicNumRange>();

                AtomicNumRange ret = sptr.getValue().intersect(thisptr.getValue());

                //LOGGER.debug("*** " + sptr);

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
        //LOGGER.debug("DONE ");

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

        LOGGER.debug("ADD {} to {}", e, this);

        if(ran.size() == 0){
            AtomicNumRange cp = e;
            ran.put(cp.getMin(), cp);
            lb = cp.getMin();
            ub = cp.getMax();
            //setMin(e.getMin());
            //setMax(e.getMax());

            LOGGER.debug("added {}", cp);
            return;
        }

        LOGGER.debug("floor max for {}", e.getMax());
        // e.max >= e.min ==> florMax => ceilMin
        Map.Entry <NumCut, AtomicNumRange> floorMax = ran.floorEntry(e.getMax());


        LOGGER.debug("ceil min for {}", e.getMin());
        // e or e's direct successor
        Map.Entry <NumCut, AtomicNumRange> ceilMin = ran.ceilingEntry(e.getMin());

        AtomicNumRange fMax = null;
        AtomicNumRange cMin = null;

        if(floorMax != null) {
            fMax = floorMax.getValue();
            LOGGER.debug("FMAX : " + fMax.toString());
        }

        if(ceilMin != null) {
            cMin = ceilMin.getValue();
            LOGGER.debug("CMIN : " + cMin.toString());
        }

        if(cMin != null) {
            //LOGGER.debug("here");
            AtomicNumRange overlap = cMin.intersect(e);
            if(overlap != null) {
                NumCut min = NumCut.min(cMin.getMin(), e.getMin());
                NumCut max = (fMax != null ? NumCut.max(fMax.getMax(),e.getMax()) : e.getMax());
                AtomicNumRange newfmin = new AtomicNumRange(min,max);
                cleanupDescending(newfmin);
                LOGGER.debug("REMOVE {}", cMin);
                this.ran.remove(cMin.getMin());
                join(newfmin);
                return;
            }
        }

        if(fMax != null) {
            //LOGGER.debug("there");
            AtomicNumRange overlap = fMax.intersect(e);
            if (overlap != null) {
                NumCut min = (ceilMin != null ? NumCut.min(ceilMin.getValue().getMin(), e.getMin()) : e.getMin());
                NumCut max = NumCut.max(fMax.getMax(), e.getMax());
                LOGGER.debug("MIN " + min + " " + fMax.getMax());
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

        LOGGER.debug("JOIN {}", e);

        // We assume (in the general case) that we have the following order rfloor:e:rceil
        Map.Entry<NumCut, AtomicNumRange> ceil = ran.ceilingEntry(e.getMax());
        Map.Entry<NumCut, AtomicNumRange> floor = ran.floorEntry(e.getMin());

        AtomicNumRange rceil = null;
        AtomicNumRange rfloor = null;
        AtomicNumRange nr = new AtomicNumRange(e);

        boolean removeFloor = false;
        boolean removeCeil = false;


        if(floor != null) {
            rfloor = floor.getValue();
            LOGGER.debug("RFLOOR {} : e {}", rfloor, e);
            if(e.intersect(rfloor) != null || e.getMin().sub(1L).equals(rfloor
                    .getMax())) {
                LOGGER.debug("gmin");
                nr.setMin(rfloor.getMin());
                removeFloor = true;
            }
        }

        if(ceil != null) {
            rceil = ceil.getValue();
            LOGGER.debug("RCEIL {} : e {} : e2 {}", rceil, e, e.getMax().add
                    (1L));
            if(e.intersect(rceil) != null || e.getMax().add(1L).equals(rceil.getMin
                    ())) {
                LOGGER.debug("gmax");
                nr.setMax(rceil.getMax());
                removeCeil = true;
            }
        }

        LOGGER.debug("nr {}", nr);

        if(removeFloor) {
            LOGGER.debug("RM floor {}", rfloor);
            ran.remove(rfloor.getMin());
        }

        if(removeCeil) {
            LOGGER.debug("RM ceil {}", rceil);
            ran.remove(rceil.getMin());
        }

        if(nr.getMin().isSmallerThan(getMin())) {
            lb = nr.getMin();
        }

        if(nr.getMax().isGreaterThan(getMax())) {
            ub = nr.getMax();
        }

        ran.put(nr.getMin(), nr);

    }

    private void cleanupDescending(AtomicNumRange from) {
        LOGGER.debug("cleanup desc " + from) ;
        Map.Entry<NumCut, AtomicNumRange> todel = ran.higherEntry(from.getMin
                ());

        while (todel != null &&
                from.subsumes(todel.getValue())&&
                !todel.getValue().equals(from)) {

            ran.remove(todel.getKey());
            todel = ran.higherEntry(todel.getValue().getMin());
        }
    }

    private void cleanupAscending(AtomicNumRange from) {
        LOGGER.debug("cleanup asc " + from);
        Map.Entry<NumCut, AtomicNumRange> todel = ran.lowerEntry(from.getMax());

        while (todel != null &&
                from.subsumes(todel.getValue())&&
                !todel.getValue().equals(from)) {

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


        if(!getMin().equals(rs.getMin()) || !getMax().equals(rs.getMax()))
            return false;


        for(Map.Entry<NumCut, AtomicNumRange> e : ran.entrySet()) {

            AtomicNumRange other = rs.ran.get(e.getKey());

            if(!e.getValue().equals(other))
                return false;

        }
        return true;
    }

    public TreeMap<NumCut, AtomicNumRange> getRangeMap() {
        //LOGGER.debug("SI {}", this.ran.size());
        return this.ran;
    }

    public SimpleAutomaton toAutomaton() {

        LOGGER.debug("-- {}", this);
        SimpleAutomaton automaton = new SimpleAutomaton(".*");
        for(Map.Entry<NumCut, AtomicNumRange> e : this.ran.entrySet()) {
            if(automaton == null) {
                automaton = e.getValue().getAutomaton();
            } else {
                automaton = automaton.union(e.getValue().getAutomaton());
            }
        }
        return automaton;
    }

    public SimpleAutomaton toApproxLenAutomaton() {
        LOGGER.debug("get approximate automaton");
        SimpleAutomaton a =  new AtomicNumRange(getMin(), getMax()).getLenAutomaton();
        LOGGER.debug("return approx automaotn");
        return a;
    }

    public NumRange numadd(NumRange nr) {
        NumRange ret = new NumRange();
        for(Map.Entry<NumCut, AtomicNumRange> eout : this.ran.entrySet()) {
            for(Map.Entry<NumCut, AtomicNumRange> ein : nr.ran.entrySet()) {

                AtomicNumRange sum = eout.getValue().numadd(ein.getValue());

                LOGGER.debug(eout.getValue() + "+" + ein.getValue() + "=" + sum);
                ret.add(sum);
            }
        }
        return ret;
    }

    public NumRange numsub(NumRange nr) {
        NumRange ret = new NumRange();
        for(Map.Entry<NumCut, AtomicNumRange> eout : this.ran.entrySet()) {
            for(Map.Entry<NumCut, AtomicNumRange> ein : nr.ran.entrySet()) {

                AtomicNumRange sum = eout.getValue().numsub(ein.getValue());

                LOGGER.debug(eout.getValue() + "-" + ein.getValue() + "=" + sum);
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
        for(Map.Entry<NumCut, AtomicNumRange> e : this.ran.entrySet()) {
            if(k++ > 0) {
                sb.append(",");
            }
            sb.append(e.getValue().toString());
        }
        sb.append("]{" + getMin() + "," + getMax() + "}" );
        return sb.toString();
    }


    @Override
    public NumRange clone() {
        return new NumRange(this);
    }

    public void setMin(NumCut min) {

        assert min.isSmallerEqualsThan(getMax());
        NumRange nr = new NumRange(new AtomicNumRange(min, getMax()));
        NumRange isect = this.intersect(nr);
        this.ran.clear();
        this.ran.putAll(isect.ran);
    }

    public void setMax(NumCut max) {
        assert getMin().isSmallerEqualsThan(getMax());
        NumRange nr = new NumRange(new AtomicNumRange(getMin(), max));
        NumRange isect = this.intersect(nr);
        this.ran.clear();
        this.ran.putAll(isect.ran);
    }

    public NumCut getDiff() {
        return new NumCut(getMax().sub(getMin()).getEndpoint());
    }
}

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

public class BooleanRange extends AtomicNumRange {

    final static Logger LOGGER = LoggerFactory.getLogger(BooleanRange.class);

    public BooleanRange() {
        super(BooleanCut.TRUE, BooleanCut.FALSE);
    }

    public BooleanRange(NumCut min, NumCut max) {
        super(min,max);
    }

    public BooleanRange(BooleanRange br) {
        super(br.getMin(),br.getMax());
    }

    public BooleanRange(BooleanCut val) {
        super(val,val);
    }

    public BooleanRange or(BooleanRange other) {

        NumCut newmin = (other.getMin().add(getMin())).div(2L);
        NumCut newmax = (other.getMax().add(getMax())).div(2L);

        return new BooleanRange(newmin, newmax);
    }

    public BooleanRange and(BooleanRange other) {

        NumCut newmin = (other.getMin().add(getMin()).isGreaterThan(new
                NumCut(0L))) ?
                BooleanCut.FALSE.clone() : BooleanCut.TRUE.clone();
        NumCut newmax = (other.getMax().add(getMax()).isGreaterThan(new
                NumCut(0L))) ?
                BooleanCut.FALSE.clone() : BooleanCut.TRUE.clone();

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
            cp.setMax(BooleanCut.FALSE.clone());
            cp.setMin(BooleanCut.FALSE.clone());
        } else {
            cp.setMax(BooleanCut.TRUE.clone());
            cp.setMin(BooleanCut.TRUE.clone());
        }
        return cp;
    }

    public boolean isAlwaysTrue() {
        return (getMax().equals(BooleanCut.TRUE) && getMin().equals(getMax()));
    }

    public boolean isAlwaysFalse() {
        return (getMax().equals(BooleanCut.FALSE) && getMin().equals(getMax()));
    }

    public boolean isCatState() {
        return (getMin().equals(BooleanCut.TRUE) && getMax().equals(BooleanCut
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

        if(getMin().equals(BooleanCut.TRUE)) {
            sb.append("T");
        }

        if(getMax().equals(BooleanCut.FALSE)) {

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

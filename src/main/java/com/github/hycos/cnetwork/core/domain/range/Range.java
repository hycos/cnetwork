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
import com.github.hycos.cnetwork.core.domain.DomainInterface;

public abstract class Range implements DomainInterface<Range> {

    private final static Logger LOGGER = LoggerFactory.getLogger(Range.class);

    protected NumCut lb;
    protected NumCut ub;

    public Range(NumCut min, NumCut max) {

        //LOGGER.debug("new ran min:{} max:{}", min, max);
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

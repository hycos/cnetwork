package org.snt.cnetwork.core.domain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class NumCut extends Cut<Long> implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(NumCut.class);

    public static NumCut max(NumCut a, NumCut b) {
        LOGGER.debug("NumCut max {} -- {}", a, b);
        if(a.isGreaterThan(b)) {
            LOGGER.debug("1");
            return a;
        } else {
            LOGGER.debug("2");
            return b;
        }
    }

    public static NumCut min(NumCut a, NumCut b) {
        LOGGER.debug("NumCut min {} -- {}", a, b);
        if(a.isSmallerThan(b)) {
            LOGGER.debug("1");
            return a;
        } else {
            LOGGER.debug("2");
            return b;
        }
    }

    public static NumCut abs(NumCut a) {
        return a.isSmallerThan(0L) ? new NumCut(a.mul(-1L)) : new NumCut(a);
    }

    public NumCut() {
        endpoint = null;
    }

    public NumCut(Long c) {
        super(c);
    }

    public NumCut(Cut<Long> sub) {
        super(sub);
    }



    public boolean isSmallerThan(Cut<Long> value) {
        if(value instanceof AboveAll)
            return true;
        if(value instanceof BelowAll)
            return false;

        return super.isSmallerThan(value);
    }
    public boolean isSmallerEqualsThan(Cut<Long> value) {
        if(value instanceof AboveAll)
            return true;
        if(value instanceof BelowAll)
            return false;

        return super.isSmallerEqualsThan(value);
    }

    public boolean isGreaterThan(Cut<Long> value){
        if(value instanceof BelowAll)
            return true;
        if(value instanceof AboveAll)
            return false;
        return super.isGreaterThan(value);
    }

    public boolean isGreaterEqualsThan(Cut<Long> value){
        if(value instanceof BelowAll)
            return true;
        if(value instanceof AboveAll)
            return false;
        return super.isGreaterEqualsThan(value);
    }

    @Override
    public boolean isSmallerThan(Long value) {
        return endpoint < value;
    }

    @Override
    public boolean isSmallerEqualsThan(Long value) {
        return endpoint <= value;
    }

    @Override
    public boolean isGreaterThan(Long value) {
        return endpoint > value;
    }

    @Override
    public boolean isGreaterEqualsThan(Long value) {
        return endpoint >= value;
    }

    @Override
    public boolean isAboveAll() {
        return false;
    }

    @Override
    public boolean isBelowAll() {
        return false;
    }

    @Override
    public NumCut sub(Cut<Long> val) {
        return sub(val.endpoint);
    }

    @Override
    public NumCut add(Cut<Long> val) {
        return add(val.endpoint);
    }

    @Override
    public NumCut div(Cut<Long> val) {
        return div(val.endpoint);
    }

    @Override
    public NumCut mul(Cut<Long> val) {
        return mul(val.endpoint);
    }

    @Override
    public NumCut diff(Cut<Long> val) {
        return diff(val.endpoint);
    }


    public NumCut sub(Long val) {
        return new NumCut(endpoint - val);
    }


    public NumCut add(Long val) {
        return new NumCut(endpoint + val);
    }


    public NumCut div(Long val) {
        return new NumCut(endpoint/val);
    }

    public NumCut mul(Long val) {
        return new NumCut(endpoint*val);
    }

    public NumCut diff(Long val) {
        return abs(sub(val));
    }

    @Override
    public boolean isFixed() {
        return true;
    }

    @Override
    public NumCut clone() {
        return new NumCut(this.endpoint);
    }

    @Override
    public String toString() {
       return this.endpoint < 0 ? this.endpoint.toString() : "+" + this
               .endpoint.toString();
    }

    @Override
    public boolean equals(Object o) {
        if((o instanceof NumCut)) {
            NumCut a = (NumCut)o;
            return this.endpoint.equals(a.endpoint);
        }
        return false;
    }
}

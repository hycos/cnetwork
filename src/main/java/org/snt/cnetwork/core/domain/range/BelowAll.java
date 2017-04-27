package org.snt.cnetwork.core.domain.range;

public class BelowAll extends NumCut {

    public BelowAll(Long l) {
        super(l);
    }

    public BelowAll() {
        this(0L);
    }

    @Override
    public boolean isSmallerThan(Cut<Long> val) {
        if (val instanceof BelowAll) {
            BelowAll l = (BelowAll)val;
            return this.endpoint < l.endpoint;
        }
        return isSmallerThan(val.endpoint);
    }

    @Override
    public boolean isSmallerEqualsThan(Cut<Long> val) {
        if (val instanceof BelowAll) {
            BelowAll l = (BelowAll)val;
            return this.endpoint <= l.endpoint;
        }
        return isSmallerEqualsThan(val.endpoint);
    }

    @Override
    public boolean isGreaterThan(Cut<Long> val) {
        if (val instanceof BelowAll) {
            BelowAll l = (BelowAll)val;
            return this.endpoint > l.endpoint;
        }
        return isGreaterThan(val.endpoint);
    }

    @Override
    public boolean isGreaterEqualsThan(Cut<Long> val) {
        if (val instanceof BelowAll) {
            BelowAll l = (BelowAll)val;
            return this.endpoint >= l.endpoint;
        }
        return isGreaterEqualsThan(val.endpoint);
    }

    @Override
    public boolean isSmallerThan(Long value) {
        return true;
    }

    @Override
    public boolean isSmallerEqualsThan(Long value) {
        return true;
    }

    @Override
    public boolean isGreaterThan(Long value) {
        return false;
    }

    @Override
    public boolean isGreaterEqualsThan(Long value) {
        return false;
    }

    @Override
    public AboveAll negate() {
        return new AboveAll(-endpoint);
    }

    @Override
    public NumCut sub(Long val) {
        return new BelowAll(endpoint - val);
    }

    @Override
    public NumCut sub(Cut<Long> val) {
        if(val instanceof BelowAll) {
            return new NumCut(endpoint - val.endpoint);
        } else {
            return sub(val.endpoint);
        }
    }

    @Override
    public NumCut add(Long val) {
        return new BelowAll(endpoint + val);
    }

    @Override
    public NumCut add(Cut<Long> val) {
        LOGGER.debug("add {} to {}", val, this);
        if(val instanceof AboveAll) {
            return new NumCut(endpoint + val.endpoint);
        } else {
            return add(val.endpoint);
        }
    }


    @Override
    public String toString() {
        return "-\u221e" + super.toString();
    }

    @Override
    public boolean isFixed() {
        return false;
    }

    @Override
    public int hashCode() {
        return endpoint.hashCode();
    }

    @Override
    public NumCut clone() {
        return new BelowAll(this.endpoint);
    }

    @Override
    public boolean isBelowAll() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if((o instanceof BelowAll)) {
            BelowAll a = (BelowAll)o;
            return this.endpoint.equals(a.endpoint);
        }
        return false;
    }

}
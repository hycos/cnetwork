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

public class AboveAll extends NumCut {

    public AboveAll(Long c) {
        super(c);
    }

    public AboveAll() {
        super(0L);
    }

    @Override
    public boolean isSmallerThan(Cut<Long> val) {
        if (val instanceof AboveAll) {
            AboveAll l = (AboveAll)val;
            return this.endpoint < l.endpoint;
        }
        return isSmallerThan(val.endpoint);
    }

    @Override
    public boolean isSmallerEqualsThan(Cut<Long> val) {
        if (val instanceof AboveAll) {
            AboveAll l = (AboveAll)val;
            return this.endpoint <= l.endpoint;
        }
        return isSmallerEqualsThan(val.endpoint);
    }

    @Override
    public boolean isGreaterThan(Cut<Long> val) {
        if (val instanceof AboveAll) {
            AboveAll l = (AboveAll)val;
            return this.endpoint > l.endpoint;
        }
        return isGreaterThan(val.endpoint);
    }

    @Override
    public boolean isGreaterEqualsThan(Cut<Long> val) {
        if (val instanceof AboveAll) {
            AboveAll l = (AboveAll)val;
            return this.endpoint >= l.endpoint;
        }
        return isGreaterEqualsThan(val.endpoint);
    }

    @Override
    public boolean isSmallerThan(Long value) {
        return false;
    }

    @Override
    public boolean isSmallerEqualsThan(Long value) {
        return false;
    }

    @Override
    public boolean isGreaterThan(Long value) {
        return true;
    }

    @Override
    public boolean isGreaterEqualsThan(Long value) {
        return true;
    }

    @Override
    public NumCut sub(Long val) {
        if(endpoint != null)
            return new AboveAll(endpoint - val);

        return new AboveAll(- val);
    }

    @Override
    public NumCut sub(Cut<Long> val) {
        if(val instanceof AboveAll) {
            return new NumCut(endpoint - val.endpoint);
        } else {
            return sub(val.endpoint);
        }
    }

    @Override
    public NumCut add(Long val) {
        return new AboveAll(endpoint + val);
    }

    @Override
    public NumCut add(Cut<Long> val) {
        if(val instanceof BelowAll) {
            return new NumCut(endpoint + val.endpoint);
        } else {
            return add(val.endpoint);
        }
    }

    @Override
    public BelowAll negate() {
        return new BelowAll(-endpoint);
    }

    @Override
    public String toString() {
        return "+\u221e" + super.toString();
    }

    @Override
    public boolean isFixed() {
        return false;
    }

    @Override
    public NumCut clone() {
        return new AboveAll(this.endpoint);
    }

    @Override
    public boolean isAboveAll() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if((o instanceof AboveAll)) {
            AboveAll a = (AboveAll)o;
            return this.endpoint.equals(a.endpoint);
        }
        return false;
    }


}
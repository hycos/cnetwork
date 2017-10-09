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


import java.io.Serializable;

abstract class Cut<C extends Comparable> implements Comparable<Cut<C>>,
        Serializable {

    protected C endpoint;

    protected Cut(Cut<C> c) {
        this.endpoint = c.endpoint;
    }

    protected Cut(C c) {
        this.endpoint = c;
    }

    protected Cut() {
    }


    public abstract boolean isSmallerThan(Cut<C> value);
    public abstract boolean isSmallerEqualsThan(Cut<C> value);
    public abstract boolean isGreaterThan(Cut<C> value);
    public abstract boolean isGreaterEqualsThan(Cut<C> value);

    public abstract boolean isAboveAll();
    public abstract boolean isBelowAll();

    public abstract Cut<C> sub(Cut<C> val);
    public abstract Cut<C> add(Cut<C> val);
    public abstract Cut<C> div(Cut<C> val);
    public abstract Cut<C> mul(Cut<C> val);
    public abstract Cut<C> diff(Cut<C> val);
    public abstract Cut<C> negate();

    @Override
    public int hashCode() {
        return toString().hashCode();
    }
    // note: overriden by {BELOW,ABOVE}_ALL
    @Override
    public int compareTo(Cut<C> o) {
        if (isGreaterThan(o)) {
            return 1;
        }
        if (isSmallerThan(o)) {
            return -1;
        }
        return 0;
    }


    public C getEndpoint() {
        return endpoint;
    }

    @Override
    public abstract boolean equals(Object obj);

    abstract boolean isFixed();

}

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

public class BooleanCut extends NumCut {

    private final String sval;

    public static BooleanCut TRUE = new BooleanCut(0,"true");
    public static BooleanCut FALSE = new BooleanCut(1,"false");


    public BooleanCut(long ival, String sval) {
        super(ival);
        this.sval = sval;
    }

    public long getId() {
        return this.endpoint;
    }

    public String toString() {
        return this.sval;
    }

    public static BooleanCut KindFromString(String kind) {
        switch(kind) {
            case "true" : return TRUE.clone();
            case "false" : return FALSE.clone();
            case "0": return TRUE.clone();
            case "1": return FALSE.clone();
        }
        assert(false);
        return null;

    }

    @Override
    public BooleanCut negate() {
        if(isTrue()){
            return FALSE.clone();
        }
        return TRUE.clone();
    }


    @Override
    public BooleanCut clone() {
        return new BooleanCut(this.endpoint, this.sval);
    }

    public String getValue() {
        return this.sval;
    }

    public boolean isTrue(){
        return this.endpoint.equals(0L);
    }

    public boolean isFalse(){
        return !isTrue();
    }


}

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

package com.github.hycos.cnetwork.core.graph;

import com.github.hycos.cnetwork.api.EdgeKindInterface;

public enum EdgeKind implements EdgeKindInterface {
    UNKNOWN(0,"unknown"),
    PAR_IN(1,"parin");

    private static final long serialVersionUID = -8824222790217211310L;

    private final String value;
    private int ival;

    EdgeKind(int ival, String val) {
        this.value = val;
        this.ival = ival;
    }

    public static EdgeKind KindFromString(String kind) {

        switch (kind) {
            case "parin":
                return PAR_IN;
            case "":
                return UNKNOWN;
        }
        // should never ever happen
        assert (false);
        return null;
    }


    public String getValue() {
        return this.value;
    }


    public int getId() {
        return ival;
    }



}


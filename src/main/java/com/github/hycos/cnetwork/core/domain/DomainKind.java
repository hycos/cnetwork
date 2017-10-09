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

package com.github.hycos.cnetwork.core.domain;


public enum DomainKind {

    UNKNOWN(0,"unknown"),
    NUMERIC_Z(1,"numeric_z"),
    NUMERIC_N(2,"numeric_n"),
    NUMERIC_NM1(3,"numeric_nm1"),
    STRING(4,"string"),
    STRING_UPPER(5, "string_upper"),
    STRING_LOWER(6, "string_lower"),
    STRING_TRIMMED(7, "string_trimmed"),
    BOOLEAN(8,"boolean");

    private final String sval;
    private final int ival;

    DomainKind(int ival, String sval) {
        this.sval = sval;
        this.ival = ival;
    }

    public int getId() {
        return this.ival;
    }

    public String toString() {
        return this.sval;
    }

    public static DomainKind ReturnTypeFromString(String kind) {
        switch(kind) {
            case "unknown": return UNKNOWN;
            case "numeric_z": return NUMERIC_Z;
            case "numeric_n": return NUMERIC_N;
            case "numeric_nm1": return NUMERIC_NM1;
            case "string": return STRING;
            case "string_lower": return STRING_LOWER;
            case "string_upper": return STRING_UPPER;
            case "string_trimmed": return STRING_TRIMMED;
            case "boolean": return BOOLEAN;
        }
        // should never ever happen
        assert(false);
        return null;
    }

    public boolean isNumeric() {
        return this == NUMERIC_Z || this == NUMERIC_NM1 || this == NUMERIC_N;
    }

    public boolean isBoolean() {
        return this == BOOLEAN;
    }

    public boolean isString() {
        return this == STRING || this == STRING_LOWER || this == STRING_UPPER ||
                this == STRING_TRIMMED;
    }

}


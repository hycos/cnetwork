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


import com.github.hycos.cnetwork.api.NodeKindInterface;
import com.github.hycos.cnetwork.api.domctrl.DomainKind;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public enum DefaultNodeKind implements NodeKindInterface {

    UNKNOWN("unkown"),

    // Operands
    NUMVAR("numvar"),
    STRVAR("strvar"),
    STRREXP("strexp"),
    NUMLIT("numlit"),
    STRLIT("strlit"),
    BOOLLIT("boollit"),
    BOOLVAR("boolvar"),

    XMLI("xmli"),
    SQLISTR("sqlistr"),
    SQLINUM("sqlinum"),
    XPATHSTR("xpathstr"),
    XPATHNUM("xpathnum"),
    LDAPI("ldapi"),
    XSS("xss"),
    URLI("urli"),

    // Operations
    EXTERNAL("external"),
    // overloaded operators
    EQUALS("=="),
    STR_EQUALS("=="),
    NUM_EQUALS("=="),
    BOOL_EQUALS("=="),
    STR_EQUALSIC("~~"),

    NEQUALS("!="),
    STR_NEQUALS("!="),
    NUM_NEQUALS("!="),
    BOOL_NEQUALS("!="),
    STR_NEQUALSIC("!~"),


    SMALLER("<"),
    GREATER(">"),
    SMALLEREQ("<="),
    GREATEREQ(">="),
    MATCHES("matches"),
    STARTSWITH("startswith"),
    ENDSWITH("endswith"),
    CONTAINS("contains"),

    OR("or"),
    AND("and"),
    XOR("xor"),
    NOT("not"),
    ITE("ite"),
    IMPLIES("implies"),

    SUBSTR("substr"),
    INDEXOF("indexof"),
    LASTINDEXOF("lastindexof"),
    TOLOWER("tolower"),
    TOUPPER("toupper"),
    CONCAT("concat"),
    TRIM("trim"),
    LEN("len"),
    REPLACE("replace"),
    STRINV("strinv"),
    CHARAT("charat"),

    TOINT("toint"),
    TOSTR("tostr"),

    ADD("add"),
    SUB("sub"),

    APACHE_ESCHTML("apache_eschtml"),
    APACHE_UESCHTML("apache_ueschtml"),
    APACHE_ESCXML10("apache_escxml10"),
    APACHE_ESCXML11("apache_escxml11"),
    APACHE_ESCJSON("apache_escjson"),
    APACHE_ESCECMA("apache_escecma"),

    ESAPI_ESCLDAP("esapi_escldap"),
    ESAPI_ESCDN("esapi_escdn"),
    ESAPI_ESCHTML("esapi_eschtml"),
    ESAPI_ESCHTMLATTR("esapi_eschtmlattr"),
    ESAPI_ESCXML("esapi_escxml"),
    ESAPI_ESCXMLATTR("esapi_escxmlattr"),
    ESAPI_ESCXPATH("esapi_escxpath"),
    ESAPI_ESCSQL("esapi_escsql"),

    EMTPY("emtpy"),

    // special search procedure
    SEARCH("search"),

    DIV("div");

    final static Logger LOGGER = LoggerFactory.getLogger(DefaultNodeKind.class);

    private final String sval;
    //private DomainKind dkind;


    DefaultNodeKind(String sval) {
        this.sval = sval;
        //this.dkind = type;
    }


    public String toString() {
        return this.sval;
    }

    public static DefaultNodeKind KindFromString(String kind) {
        switch(kind) {
            // operations
            case "==": return EQUALS;
            case "!=": return NEQUALS;

            case "<": return SMALLER;
            case ">": return GREATER;
            case "<=": return SMALLEREQ;
            case ">=": return GREATEREQ;

            case "~~": return STR_EQUALSIC;
            case "!~": return STR_NEQUALSIC;

            case "matches": return MATCHES;
            case "startswith": return STARTSWITH;
            case "endswith": return ENDSWITH;
            case "contains": return CONTAINS;

            case "or" : return OR;
            case "xor": return XOR;
            case "and": return AND;
            case "not": return NOT;
            case "ite": return ITE;
            case "implies": return IMPLIES;

            case "substr": return SUBSTR;
            case "indexof": return INDEXOF;
            case "lastindexof": return LASTINDEXOF;

            case "tolower": return TOLOWER;
            case "toupper": return TOUPPER;

            case "concat": return CONCAT;
            case "trim": return TRIM;
            case "len": return LEN;
            case "collapse": return REPLACE;
            case "strinv": return STRINV;
            case "charat": return CHARAT;

            case "valueof": return TOINT;
            case "tostr": return TOSTR;
            case "add": return ADD;
            case "sub": return SUB;
            case "external": return EXTERNAL;

            case "apache_eschtml": return APACHE_ESCHTML;
            case "apache_ueschtml": return APACHE_UESCHTML;
            case "apache_escxml10": return APACHE_ESCXML10;
            case "apache_escxml11": return APACHE_ESCXML11;
            case "apache_escjson": return APACHE_ESCJSON;
            case "apache_escecma": return APACHE_ESCECMA;

            case "esapi_escldap": return ESAPI_ESCLDAP;
            case "esapi_escdn": return ESAPI_ESCDN;
            case "esapi_eschtml": return ESAPI_ESCHTML;
            case "esapi_eschtmlattr": return ESAPI_ESCHTMLATTR;
            case "esapi_escxml": return ESAPI_ESCXML;
            case "esapi_escxmlattr": return ESAPI_ESCXMLATTR;
            case "esapi_escxpath": return ESAPI_ESCXPATH;
            case "esapi_escsql": return ESAPI_ESCSQL;

            case "emtpy": return EMTPY;

            // meta operation
            case "search" : return SEARCH;


            // operands
            case "strlit": return STRLIT;
            case "strvar": return STRVAR;
            case "strexp": return STRREXP;

            case "numlit": return NUMLIT;
            case "numvar": return NUMVAR;

            case "boollit": return BOOLLIT;
            case "boolvar": return BOOLVAR;

            case "xmli": return XMLI;
            case "sqlistr": return SQLISTR;
            case "sqlinum": return SQLINUM;
            case "xpathstr": return XPATHSTR;
            case "xpathnum": return XPATHNUM;
            case "ldapi": return LDAPI;
            case "xss": return XSS;
            case "urli": return URLI;

            case "replace": return REPLACE;

            //default:
            //    return UNKNOWN;
        }
        return UNKNOWN;
    }

    public String getDesc() {
        if(this == EQUALS)
            return "eq";
        else if (this == STR_EQUALS)
            return "seq";
        else if (this == BOOL_EQUALS)
            return "beq";
        else return this.getValue();
    }


    public boolean isSanitizer() {
        return this == APACHE_ESCHTML || this == APACHE_UESCHTML ||
                this == APACHE_ESCXML10 || this == APACHE_ESCXML11 ||
                this == APACHE_ESCJSON || this == APACHE_ESCECMA ||
                this == ESAPI_ESCLDAP || this == ESAPI_ESCDN ||
                this == ESAPI_ESCHTML || this == ESAPI_ESCHTMLATTR ||
                this == ESAPI_ESCXML || this == ESAPI_ESCXMLATTR ||
                this == ESAPI_ESCXPATH || this == ESAPI_ESCSQL;
    }

    public boolean isComparative() {
        return this == EQUALS || this == NEQUALS || this == STR_NEQUALS || this == STR_EQUALS ||
                this == NUM_NEQUALS || this == NUM_EQUALS || this == BOOL_EQUALS ||
                this == BOOL_NEQUALS || this == STR_NEQUALSIC || this ==
                STR_EQUALSIC || this == EMTPY || this == MATCHES || this ==
                OR || this == AND || this == XOR || this == IMPLIES || this
                == NOT || this == CONTAINS || this == GREATEREQ || this ==
                SMALLEREQ || this == SMALLER || this == GREATER || this ==
                STARTSWITH || this == ENDSWITH || this == ITE;
    }


    public boolean isEquality() {
        return this == EQUALS || this == STR_EQUALS || this == NUM_EQUALS ||
                this == BOOL_EQUALS;
    }

    public boolean isInequality() {
        return this == NEQUALS || this == STR_NEQUALS || this == NUM_NEQUALS ||
                this == BOOL_NEQUALS || this == STR_NEQUALSIC;
    }

    public boolean isOperation() {
        return isComparative() || isSanitizer() || this == SUBSTR || this == INDEXOF || this == TOLOWER || this
                == TOUPPER || this == CONCAT || this == TRIM|| this == LEN ||
                this == REPLACE || this == STRINV || this == CHARAT || this
                == TOINT || this == TOSTR || this == ADD || this == SUB ||
                this == EXTERNAL || this == LASTINDEXOF;
    }

    public boolean isBranch() {
        return this == ITE;
    }

    public boolean isLiteral() {
        return this == NUMLIT || this == STRLIT || this == BOOLLIT ;
    }

//    public boolean isNumeric() {
//        return this.dkind.isNumeric();
//    }
//
//    public boolean isBoolean() { return this.dkind.isBoolean(); }
//
//    public boolean isString() {
//        return this.dkind.isString();
//    }

    public boolean isThreatModel() {
        return this == XMLI || this == SQLISTR || this == SQLINUM ||
                this == XPATHSTR || this == XPATHNUM || this == LDAPI || this == XSS;
    }

    public boolean isRegex() {
        return this == STRREXP;
    }

    public boolean isVariable() { return !isLiteral(); }

    public boolean isOperand() {
        return !isOperation();
    }

    //public DomainKind getDomainKind() {
    //    return this.dkind;
    //}

    public String getValue() {
        return this.sval;
    }

    public boolean isNumeric() {
        return this == ADD || this == SUB || this == DIV || this == NUMLIT ||
                this == NUMVAR || this == INDEXOF || this == LEN;
    }


    public boolean isString() {
        return isComparative() || isSanitizer() || this == SUBSTR ||
                this == INDEXOF || this == TOLOWER || this
                == TOUPPER || this == CONCAT || this == TRIM|| this == LEN ||
                this == REPLACE || this == STRINV || this == CHARAT || this
                == TOINT || this == TOSTR || this == LASTINDEXOF || this ==
                STRVAR || this == STRLIT;
    }

    public boolean isBoolean() {
        return this == XOR || this == AND || this == OR || this == ITE ||
                this == BOOLVAR || this == BOOLLIT;
    }


    public DomainKind getDomainKind() {
        return DomainKind.UNKNOWN;
    }



}
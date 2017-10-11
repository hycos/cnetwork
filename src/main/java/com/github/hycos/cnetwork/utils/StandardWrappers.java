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

package com.github.hycos.cnetwork.utils;

import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.sig.JavaMethodSignature;
import org.apache.commons.lang3.StringEscapeUtils;
import org.owasp.esapi.codecs.OracleCodec;

public class StandardWrappers {

    public static JavaMethodSignature getSigForOperation(NodeKind kind) {
        return JavaMethodSignature.fromString(getSigStringForOperation(kind));
    }

    private static String getSigStringForOperation(NodeKind kind) {
        switch(kind) {
            case UNKNOWN:
            case EXTERNAL:
            case EQUALS:
            case STRINV:
            case SEARCH:
            case NEQUALS:
            case ITE:
            case DIV:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.noOp()" +
                        "Ljava/lang/String;";
            case STR_EQUALS:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.strEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case STR_EQUALSIC:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.strEqIc(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case STR_NEQUALSIC:
                return  "com.github.hycos.cnetwork.utils.StandardWrappers.strNeqIc(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case NUM_EQUALS:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.numEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case BOOL_EQUALS:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.boolEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case STR_NEQUALS:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.strNeq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case NUM_NEQUALS:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.numNeq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case BOOL_NEQUALS:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.boolNeq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case SMALLER:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.numSmaller(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case GREATER:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.numGreater(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case SMALLEREQ:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.numSmallerEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case GREATEREQ:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.numGreaterEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case MATCHES:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.matches(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case STARTSWITH:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.startsWith(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case ENDSWITH:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.endsWith(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case CONTAINS:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.contains(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case OR:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.or(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case AND:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.and(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case XOR:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.xor(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case NOT:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.not(Ljava/lang/String;)Ljava/lang/String;";
            case IMPLIES:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.implies" +
                        "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case SUBSTR:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.substring(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case INDEXOF:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.indexof(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case LASTINDEXOF:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.lastindexof" +
                        "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case TOLOWER:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.toLower(Ljava/lang/String;)Ljava/lang/String;";
            case TOUPPER:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.toUpper(Ljava/lang/String;)Ljava/lang/String;";
            case CONCAT:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.concat(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case TRIM:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.trim(Ljava/lang/String;)Ljava/lang/String;";
            case LEN:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.len(Ljava/lang/String;)Ljava/lang/String;";
            case REPLACE:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.replace" +
                        "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case TOINT:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.valueOf(Ljava/lang/String;)Ljava/lang/String;";
            case TOSTR:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.toStr(Ljava/lang/String;)Ljava/lang/String;";
            case ADD:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.add(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case SUB:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.sub(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case APACHE_ESCHTML:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.apacheEscapeHTML(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_UESCHTML:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.apacheUnescapeHTML(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_ESCXML10:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.apacheEscapeXML10(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_ESCXML11:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.apacheEscapeXML11(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_ESCJSON:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.apacheEscapeJSON(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_ESCECMA:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.apacheEscapeEcmaScript(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCLDAP:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.owaspEncodeForLDAP(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCDN:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.owaspEncodeForDN(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCHTML:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.owaspEncodeForHTML(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCHTMLATTR:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.owaspEncodeForHTMLAttribute(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCXML:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.owaspEncodeForXML(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCXMLATTR:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.owaspEncodeForXMLAttribute(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCXPATH:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.owaspEncodeForXPath(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCSQL:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.owaspEncodeForSQL(Ljava/lang/String)Ljava/lang/String;";

            case EMTPY:
                return "com.github.hycos.cnetwork.utils.StandardWrappers.empty(Ljava/lang/String)Ljava/lang/String;";

        }
        assert(false);
        return "";
    }

    public static String numGreater(String a, String b) {
        return Long.parseLong(a) > Long.parseLong(b) ? "true" : "false" ;
    }

    public static String numSmaller(String a, String b) {
        return Long.parseLong(a) < Long.parseLong(b) ? "true" : "false" ;
    }

    public static String numGreaterEq(String a, String b) {
        return Long.parseLong(a) >= Long.parseLong(b) ? "true" : "false" ;
    }

    public static String numSmallerEq(String a, String b) {
        return Long.parseLong(a) <= Long.parseLong(b) ? "true" : "false" ;
    }

    public static String numEq(String a, String b) {
        return Long.parseLong(a) == Long.parseLong(b) ? "true" : "false" ;
    }

    public static String strEq(String a, String b) {
        return a.equals(b) ? "true" : "false" ;
    }

    public static String boolEq(String a, String b) {
        return Boolean.parseBoolean(a) == Boolean.parseBoolean(a) ? "true" : "false" ;
    }

    public static String numNeq(String a, String b) {
        return Long.parseLong(a) == Long.parseLong(b) ? "false" : "true" ;
    }

    public static String strNeq(String a, String b) {
        return a.equals(b) ? "false" : "true" ;
    }

    public static String boolNeq(String a, String b) {
        return Boolean.parseBoolean(a) == Boolean.parseBoolean(a) ? "false" : "true" ;
    }

    public static String and(String a, String b) {
        return Boolean.parseBoolean(a) && Boolean.parseBoolean(b) ? "true" : "false" ;
    }

    public static String or(String a, String b) {
        return Boolean.parseBoolean(a) || Boolean.parseBoolean(b) ? "true" : "false" ;
    }

    public static String xor(String a, String b) {
        return (Boolean.parseBoolean(a) && !Boolean.parseBoolean(b)) || (!Boolean.parseBoolean(a) && Boolean.parseBoolean(b)) ? "true" : "false";
    }

    public static String implies(String a, String b) {
        boolean ba = Boolean.parseBoolean(b);
        boolean aa = Boolean.parseBoolean(a);

        if(aa && !aa)
            return "false";
        else
            return "true";

    }

    public static String strEqIc(String s0, String s1) {
        boolean equals = s0.equalsIgnoreCase(s1);
        return String.valueOf(equals);
    }

    public static String strNeqIc(String s0, String s1) {
        boolean equals = !s0.equalsIgnoreCase(s1);
        return String.valueOf(equals);
    }



    public static String not(String a) {
        return Boolean.parseBoolean(a) ? "false" : "true" ;
    }

    public static String matches(String a, String b) {
        return a.matches(b) ? "true" : "false" ;
    }

    public static String contains(String a, String b) {
        return a.contains(b) ? "true" : "false" ;
    }

    public static String startWith(String a, String b) {
        return a.startsWith(b) ? "true" : "false" ;
    }

    public static String endsWith(String a, String b) {
        return a.startsWith(b) ? "true" : "false" ;
    }

    public static String toLower(String a) {
        return a.toLowerCase();
    }

    public static String toUpper(String a) {
        return a.toUpperCase();
    }

    public static String emtpy(String a) {
        return a.isEmpty() ? "true" : "false" ;
    }


    public static String add(String a, String b) {
        long result = Long.parseLong(a) + Long.parseLong(b);
        return "" + result;
    }

    public static String sub(String a, String b) {
        long result = Long.parseLong(a) - Long.parseLong(b);
        return "" + result;
    }

    public static String len(String a) {
        return "" + a.length();
    }

    public static String toStr(String a) {
        return a;
    }

    public static String valueOf(String a) {
        return a;
    }

    public static String trim(String a) {
        return a.trim();
    }

    public static String substring(String a, String b, String c) {
        return a.substring(Integer.parseInt(b), Integer.parseInt(c));
    }

    public static String replace(String a, String b, String c) {
        return a.replace(b,c);
    }

    public static String indexOf(String a, String b, String c) {
        return String.valueOf(a.indexOf(b, Integer.parseInt(c)));
    }

    public static String lastindexof(String a, String b, String c) {
        return String.valueOf(a.lastIndexOf(b, Integer.parseInt(c)));
    }

    public static String concat(String a, String b) {
        return a+b;
    }

    public static String noOp() {
        return "";
    }

    public static String owaspEncodeForSQL(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForSQL(new OracleCodec(),s);
    }

    public static String owaspEncodeForXpath(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForXPath(s);
    }

    public static String owaspEncodeForJSON(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForLDAP(s);
    }

    public static String owaspEncodeForDN(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForDN(s);
    }

    public static String owaspEncodeForHTML(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForHTML(s);
    }

    public static String owaspEncodeForHTMLAttribute(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForHTMLAttribute(s);
    }

    public static String owaspEncodeForJavaScript(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForJavaScript(s);
    }

    public static String owaspEncodeForXML(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForXML(s);
    }

    public static String owaspEncodeForXMLAttribute(String s) {
        return org.owasp.esapi.ESAPI.encoder().encodeForXMLAttribute(s);
    }

    public static String apacheEscapeHTML(String s) {
        return StringEscapeUtils.escapeHtml3(s);
    }

    public static String apacheUnescapeHTML(String s) {
        return StringEscapeUtils.unescapeHtml3(s);
    }

    public static String apacheEscapeXML10(String s) {
        return StringEscapeUtils.escapeXml10(s);
    }

    public static String apacheEscapeXML11(String s) {
        return StringEscapeUtils.escapeXml11(s);
    }

    public static String apacheUnescapeXML(String s) {
        return StringEscapeUtils.unescapeXml(s);
    }

    public static String apacheEscapeJSON(String s) {
        return StringEscapeUtils.escapeJson(s);
    }

    public static String apacheUnescapeJSON(String s) {
        return StringEscapeUtils.unescapeJson(s);
    }

    public static String apacheEscapeEcmaScript(String s) {
        return StringEscapeUtils.escapeEcmaScript(s);
    }

    public static String apacheUnescapeEcmaScript(String s) {
        return StringEscapeUtils.unescapeEcmaScript(s);
    }
}

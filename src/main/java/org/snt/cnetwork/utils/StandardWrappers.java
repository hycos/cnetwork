package org.snt.cnetwork.utils;

import org.apache.commons.lang3.StringEscapeUtils;
import org.owasp.esapi.codecs.OracleCodec;
import org.snt.cnetwork.core.OperationKind;
import org.snt.cnetwork.sig.JavaMethodSignature;

public class StandardWrappers {

    public static JavaMethodSignature getSigForOperation(OperationKind kind) {
        return JavaMethodSignature.fromString(getSigStringForOperation(kind));
    }

    private static String getSigStringForOperation(OperationKind kind) {
        switch(kind) {
            case ASSIGNMENT:
            case UNKNOWN:
            case EXTERNAL:
            case EQUALS:
            case STRINV:
            case SEARCH:
            case NEQUALS:
            case ITE:
            case DIV:
                return "org.snt.cnetwork.utils.StandardWrappers.noOp()Ljava/lang/String;";
            case STR_EQUALS:
                return "org.snt.cnetwork.utils.StandardWrappers.strEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case STR_EQUALSIC:
                return "org.snt.cnetwork.utils.StandardWrappers.strEqIc(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case STR_NEQUALSIC:
                return  "org.snt.cnetwork.utils.StandardWrappers.strNeqIc(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case NUM_EQUALS:
                return "org.snt.cnetwork.utils.StandardWrappers.numEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case BOOL_EQUALS:
                return "org.snt.cnetwork.utils.StandardWrappers.boolEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case STR_NEQUALS:
                return "org.snt.cnetwork.utils.StandardWrappers.strNeq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case NUM_NEQUALS:
                return "org.snt.cnetwork.utils.StandardWrappers.numNeq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case BOOL_NEQUALS:
                return "org.snt.cnetwork.utils.StandardWrappers.boolNeq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case SMALLER:
                return "org.snt.cnetwork.utils.StandardWrappers.numSmaller(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case GREATER:
                return "org.snt.cnetwork.utils.StandardWrappers.numGreater(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case SMALLEREQ:
                return "org.snt.cnetwork.utils.StandardWrappers.numSmallerEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case GREATEREQ:
                return "org.snt.cnetwork.utils.StandardWrappers.numGreaterEq(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case MATCHES:
                return "org.snt.cnetwork.utils.StandardWrappers.matches(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case STARTSWITH:
                return "org.snt.cnetwork.utils.StandardWrappers.startsWith(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case ENDSWITH:
                return "org.snt.cnetwork.utils.StandardWrappers.endsWith(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case CONTAINS:
                return "org.snt.cnetwork.utils.StandardWrappers.contains(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case OR:
                return "org.snt.cnetwork.utils.StandardWrappers.or(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case AND:
                return "org.snt.cnetwork.utils.StandardWrappers.and(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case XOR:
                return "org.snt.cnetwork.utils.StandardWrappers.xor(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case NOT:
                return "org.snt.cnetwork.utils.StandardWrappers.not(Ljava/lang/String;)Ljava/lang/String;";
            case IMPLIES:
                return "org.snt.cnetwork.utils.StandardWrappers.implies" +
                        "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case SUBSTR:
                return "org.snt.cnetwork.utils.StandardWrappers.substring(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case INDEXOF:
                return "org.snt.cnetwork.utils.StandardWrappers.indexof(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case TOLOWER:
                return "org.snt.cnetwork.utils.StandardWrappers.toLower(Ljava/lang/String;)Ljava/lang/String;";
            case TOUPPER:
                return "org.snt.cnetwork.utils.StandardWrappers.toUpper(Ljava/lang/String;)Ljava/lang/String;";
            case CONCAT:
                return "org.snt.cnetwork.utils.StandardWrappers.concat(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case TRIM:
                return "org.snt.cnetwork.utils.StandardWrappers.trim(Ljava/lang/String;)Ljava/lang/String;";
            case LEN:
                return "org.snt.cnetwork.utils.StandardWrappers.len(Ljava/lang/String;)Ljava/lang/String;";
            case REPLACE:
                return "org.snt.cnetwork.utils.StandardWrappers.replace(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case VALUEOF:
                return "org.snt.cnetwork.utils.StandardWrappers.valueOf(Ljava/lang/String;)Ljava/lang/String;";
            case TOSTR:
                return "org.snt.cnetwork.utils.StandardWrappers.toStr(Ljava/lang/String;)Ljava/lang/String;";
            case ADD:
                return "org.snt.cnetwork.utils.StandardWrappers.add(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case SUB:
                return "org.snt.cnetwork.utils.StandardWrappers.sub(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;";
            case APACHE_ESCHTML:
                return "org.snt.cnetwork.utils.StandardWrappers.apacheEscapeHTML(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_UESCHTML:
                return "org.snt.cnetwork.utils.StandardWrappers.apacheUnescapeHTML(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_ESCXML10:
                return "org.snt.cnetwork.utils.StandardWrappers.apacheEscapeXML10(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_ESCXML11:
                return "org.snt.cnetwork.utils.StandardWrappers.apacheEscapeXML11(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_ESCJSON:
                return "org.snt.cnetwork.utils.StandardWrappers.apacheEscapeJSON(Ljava/lang/String)Ljava/lang/String;";
            case APACHE_ESCECMA:
                return "org.snt.cnetwork.utils.StandardWrappers.apacheEscapeEcmaScript(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCLDAP:
                return "org.snt.cnetwork.utils.StandardWrappers.owaspEncodeForLDAP(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCDN:
                return "org.snt.cnetwork.utils.StandardWrappers.owaspEncodeForDN(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCHTML:
                return "org.snt.cnetwork.utils.StandardWrappers.owaspEncodeForHTML(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCHTMLATTR:
                return "org.snt.cnetwork.utils.StandardWrappers.owaspEncodeForHTMLAttribute(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCXML:
                return "org.snt.cnetwork.utils.StandardWrappers.owaspEncodeForXML(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCXMLATTR:
                return "org.snt.cnetwork.utils.StandardWrappers.owaspEncodeForXMLAttribute(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCXPATH:
                return "org.snt.cnetwork.utils.StandardWrappers.owaspEncodeForXPath(Ljava/lang/String)Ljava/lang/String;";
            case ESAPI_ESCSQL:
                return "org.snt.cnetwork.utils.StandardWrappers.owaspEncodeForSQL(Ljava/lang/String)Ljava/lang/String;";

            case EMTPY:
                return "org.snt.cnetwork.utils.StandardWrappers.empty(Ljava/lang/String)Ljava/lang/String;";

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

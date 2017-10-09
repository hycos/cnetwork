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

package com.github.hycos.cnetwork.core.consistency;

import com.github.hycos.cnetwork.core.consistency.general.*;
import com.github.hycos.cnetwork.core.consistency.specific.*;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public enum ConsistencyCheckerFactory {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(ConsistencyCheckerFactory.class);

    private static ConsistencyChecker ok = new Ok();
    private static ConsistencyChecker bbo = new BooleanBinaryOp();
    private static ConsistencyChecker buo = new BooleanUnaryOp();
    private static ConsistencyChecker bno = new BooleanNaryOp();
    private static ConsistencyChecker nbo = new NumericBinaryOp();
    private static ConsistencyChecker nco = new NumericCompOp();
    private static ConsistencyChecker sbo = new StringBinaryOp();
    private static ConsistencyChecker suo = new StringUnaryOp();
    private static ConsistencyChecker eq = new Equals();
    private static ConsistencyChecker ite = new Ite();
    private static ConsistencyChecker charat = new CharAt();
    private static ConsistencyChecker contains = new Contains();
    private static ConsistencyChecker emtpy = new Empty();
    private static ConsistencyChecker len = new Len();
    private static ConsistencyChecker matches = new Matches();
    private static ConsistencyChecker replace = new Replace();
    private static ConsistencyChecker startsWith = new StartsWith();
    private static ConsistencyChecker substr = new Substr();
    private static ConsistencyChecker tostr = new ToStr();
    private static ConsistencyChecker valueof = new ValueOf();
    private static ConsistencyChecker idxof = new IndexOf();
    private static ConsistencyChecker lastidxof = new LastIndexOf();


    public boolean isConsistent(ConstraintNetworkBuilder cb, Node n) {
        LOGGER.debug(n.getLabel());
        boolean ret = getConsistencyCheckerFor(n.getKind()).check(cb, n);
        LOGGER.debug("ret2 {}", ret);
        return ret;
    }

    public ConsistencyChecker getConsistencyCheckerFor(NodeKind kind){

        //LOGGER.debug("kind {}", kind);

        switch (kind) {

            case UNKNOWN:
            case EXTERNAL:
            case SEARCH:
                return ok;

            case AND:
            case OR:
                return bno;

            case XOR:
            case IMPLIES:
               return bbo;

            case NOT:
                return buo;

            case ITE:
                return ite;

            case NUMVAR:
            case STRVAR:
            case STRREXP:
            case NUMLIT:
            case STRLIT:
            case BOOLLIT:
            case BOOLVAR:
            case XMLI:
            case SQLISTR:
            case SQLINUM:
            case XPATHSTR:
            case XPATHNUM:
            case LDAPI:
            case XSS:
            case URLI:
                return ok;

            case EQUALS:
            case STR_EQUALS:
            case NUM_EQUALS:
            case BOOL_EQUALS:
            case STR_EQUALSIC:
            case NEQUALS:
            case STR_NEQUALS:
            case NUM_NEQUALS:
            case BOOL_NEQUALS:
            case STR_NEQUALSIC:
                return eq;

            case SMALLER:
            case GREATER:
            case SMALLEREQ:
            case GREATEREQ:
                return nco;

            case ADD:
            case SUB:
            case DIV:
                return nbo;

            case MATCHES:
                return matches;

            case STARTSWITH:
            case ENDSWITH:
                return startsWith;

            case CONTAINS:
                return contains;

            case SUBSTR:
                return substr;

            case INDEXOF:
                return idxof;

            case LASTINDEXOF:
                return lastidxof;

            case TOLOWER:
            case TOUPPER:
            case TRIM:
            case STRINV:
            case APACHE_ESCHTML:
            case APACHE_UESCHTML:
            case APACHE_ESCXML10:
            case APACHE_ESCXML11:
            case APACHE_ESCJSON:
            case APACHE_ESCECMA:
            case ESAPI_ESCLDAP:
            case ESAPI_ESCDN:
            case ESAPI_ESCHTML:
            case ESAPI_ESCHTMLATTR:
            case ESAPI_ESCXML:
            case ESAPI_ESCXMLATTR:
            case ESAPI_ESCXPATH:
            case ESAPI_ESCSQL:
                return suo;

            case EMTPY:
                return emtpy;

            case CONCAT:
                return sbo;

            case LEN:
                return len;

            case REPLACE:
                return replace;

            case CHARAT:
                return charat;

            case VALUEOF:
                return valueof;

            case TOSTR:
                return tostr;


        }

        // should never be reached
        assert false;
        return ok;
    }

    public boolean checkConsistency(ConstraintNetworkBuilder cb) {

        /**for(Node v : cb.vertexSet()) {
            if(getConsistencyCheckerFor(v.getKind()).check(cb,v) == false) {
                LOGGER.error("failed for {}:{}", v.getLabel(), v.getId());
                //LOGGER.debug(cb.getConstraintNetwork().toDot());
                return false;
            }
        }**/
        return true;
    }


}

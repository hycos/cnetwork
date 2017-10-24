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

package com.github.hycos.cnetwork.cchecktinf;

import com.github.hycos.cnetwork.api.NodeInterface;
import com.github.hycos.cnetwork.api.cchecktinf.AbstractConsistencyChecker;
import com.github.hycos.cnetwork.api.cchecktinf.ConsistencyCheckerInterface;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkInterface;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DefaultConsistencyChecker<T extends NodeInterface> implements
        ConsistencyCheckerInterface<T> {


    final static Logger LOGGER = LoggerFactory.getLogger(DefaultNodeChecker.class);

    private static AbstractConsistencyChecker ok = new DefaultNodeChecker(0);
    private static AbstractConsistencyChecker binary = new DefaultNodeChecker
            (2);
    private static AbstractConsistencyChecker unary = new DefaultNodeChecker(1);
    private static AbstractConsistencyChecker tertiary = new DefaultNodeChecker
            (3);

    private static AbstractConsistencyChecker eq = new Equals();

    public boolean isConsistent(ConstraintNetworkBuilder cb, NodeInterface n) {
        LOGGER.debug("check consistency {}", n.getKind().toString());
        boolean ret = getConsistencyCheckerFor((DefaultNodeKind)n.getKind()).check(cb, n);
        LOGGER.debug("ret2 {}", ret);
        return ret;
    }

    public AbstractConsistencyChecker getConsistencyCheckerFor(DefaultNodeKind kind){

        //LOGGER.debug("kind {}", kind);

        switch (kind) {

            case UNKNOWN:
            case EXTERNAL:
            case SEARCH:
                return ok;

            case AND:
            case OR:
                // more or equals two params
                return new DefaultNodeChecker(-1);

            case XOR:
            case IMPLIES:
                return binary;
            case NEQUALS:
            case EQUALS:
                return eq;
            case STR_EQUALS:
            case NUM_EQUALS:
            case BOOL_EQUALS:
            case STR_EQUALSIC:
            case STR_NEQUALS:
            case NUM_NEQUALS:
            case BOOL_NEQUALS:
            case STR_NEQUALSIC:
            case SMALLER:
            case GREATER:
            case SMALLEREQ:
            case GREATEREQ:
            case ADD:
            case SUB:
            case DIV:
            case MATCHES:
            case STARTSWITH:
            case ENDSWITH:
            case CONTAINS:
            case CONCAT:
            case CHARAT:
                return binary;

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

            case INDEXOF:
            case LASTINDEXOF:
                return new DefaultNodeChecker(2,3);

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
            case EMTPY:
            case NOT:
            case LEN:
            case TOINT:
            case TOSTR:
                return unary;


            case REPLACE:
            case SUBSTR:
            case ITE:
                return tertiary;



        }

        // should never be reached
        assert false;
        return ok;
    }

    @Override
    public boolean check(ConstraintNetworkInterface cb, NodeInterface n) {
        return getConsistencyCheckerFor((DefaultNodeKind)n.getKind()).check(cb,n);
    }


    @Override
    public ConsistencyCheckerInterface clone() {
        return new DefaultConsistencyChecker();
    }
}

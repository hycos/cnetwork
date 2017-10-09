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

import com.github.hycos.cnetwork.core.domain.range.BooleanRange;
import com.github.hycos.cnetwork.utils.EscapeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.domain.automaton.SimpleAutomaton;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.utils.DomainUtils;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class NodeDomain implements DomainInterface<NodeDomain> {


    final static Logger LOGGER = LoggerFactory.getLogger(NodeDomain.class);

    protected DomainKind kind = DomainKind.UNKNOWN;

    private interface DomainActionBinary {
        DomainInterface performOperation(DomainInterface a, DomainInterface b);
    }

    private interface DomainCheckBinary {
        boolean check(DomainInterface a, DomainInterface b);
    }

    private interface DomainCheckUnary {
        boolean check(DomainInterface a);
    }


    private static DomainActionBinary ISECT = (a, b) -> (DomainInterface)a.intersect(b);
    private static DomainActionBinary UNION = (a, b) -> (DomainInterface)a.union(b);
    private static DomainActionBinary MINUS = (a, b) -> (DomainInterface)a.union(b);


    private static DomainCheckBinary SUBSUMPTION = (a, b) -> a.subsumes(b);
    private static DomainCheckUnary EMPTY = a -> a.isEmpty();
    private static DomainCheckBinary SINGLETON = (a, b) -> a.isSingleton();
    private static DomainCheckBinary EQUALS = (a, b) -> a.equals(b);

    private Map<String, DomainInterface> dom = new HashMap();

    public NodeDomain(DomainKind kind, NodeDomain other) {
        this.kind = kind;
        other.dom.forEach((k,v) ->
            this.dom.put(k, (DomainInterface)v.clone())
        );
    }

    public NodeDomain(DomainKind kind, DomainInterface ... ds) {

        LOGGER.debug("kind {}", kind.toString());
        assert ds.length == 2;
        this.kind = kind;
        for(DomainInterface d : ds) {
            //LOGGER.debug("put domain {}", d.getDomainName());
            dom.put(d.getDomainName(), (DomainInterface)d.clone());
        }
    }

    public NodeDomain(DomainKind kind, Set<DomainInterface> ds) {
        this.kind = kind;
        for(DomainInterface d : ds) {
            dom.put(d.getDomainName(), (DomainInterface)d.clone());
        }
    }

    private NodeDomain applyForAll(NodeDomain y, DomainActionBinary d) {
        assert y.dom.size() == dom.size();

        Set<DomainInterface> isects = new HashSet();
        for(DomainInterface di : dom.values()){
            DomainInterface ydom = y.getDomain(di.getDomainName());
            isects.add(d.performOperation(di,ydom));
        }

        assert isects.size() == dom.size();
        return new NodeDomain(this.kind,isects);
    }

    private boolean checkForAllBinary(NodeDomain y, DomainCheckBinary d) {
        assert y.dom.size() == this.dom.size();
        for(DomainInterface i : dom.values()) {
            DomainInterface yi = y.dom.get(i.getDomainName());
            if(!d.check(i, yi))
                return false;
        }
        return true;
    }

    private boolean checkForAllUnary(DomainCheckUnary d) {
        for(DomainInterface i : dom.values()) {
            if(!d.check(i))
                return false;
        }
        return true;
    }

    public NodeDomain negate() {

        assert this.dom.containsKey("range");

        DomainInterface r = this.dom.get("range");

        assert r instanceof BooleanRange;

        BooleanRange br = (BooleanRange)r;

        BooleanRange n = br.negate();

        SimpleAutomaton a = DomainUtils.getNumAutomatonForRange(br);


        return new NodeDomain(this.kind,a,n);
    }

    @Override
    public void init(Node n) {
        for(DomainInterface i : dom.values()) {
           i.init(n);
        }
    }

    @Override
    public NodeDomain intersect(NodeDomain y) {
        return applyForAll(y, ISECT);
    }

    @Override
    public NodeDomain union(NodeDomain y) {
        return applyForAll(y, UNION);
    }

    @Override
    public NodeDomain minus(NodeDomain y) {
        return applyForAll(y, MINUS);
    }

    @Override
    public NodeDomain complement() {
        NodeDomain universe = NodeDomainFactory.INSTANCE.getDomain(this.kind);
        return universe.minus(this);
    }

    @Override
    public NodeDomain clone() {
        return new NodeDomain(this.kind, this);
    }

    @Override
    public boolean subsumes(NodeDomain y) {
        return checkForAllBinary(y, SUBSUMPTION);
    }

    @Override
    public boolean isSingleton() {
        return checkForAllBinary(this, SINGLETON);
    }

    @Override
    public boolean isEmpty() {
        return checkForAllUnary(EMPTY);
    }


    @Override
    public String getDomainName() {
        return "domain";
    }

    public DomainInterface getDomain(String name) {
        assert dom.containsKey(name);
        return dom.get(name);
    }

    public void setDomain(DomainInterface iface) {
        dom.put(iface.getDomainName(), iface);
    }

    public String getDotLabel(){
        final StringBuilder s = new StringBuilder();
        s.append("{");
        dom.forEach((v,k) -> {
            if(s.length() > 1)
                s.append(",");

            s.append(EscapeUtils.escapeSpecialCharacters(k.toString()));
        });
        s.append("}");
        return s.toString();
    }

    @Override
    public String toString() {
        final StringBuilder s = new StringBuilder();
        s.append("{");
        dom.forEach((v,k) -> {
            if(s.length() > 1)
                s.append(",");

            s.append(k.toString());
        });
        s.append("}");
        return s.toString();
    }

    @Override
    public boolean equals(Object o){

        if(!(o instanceof NodeDomain))
            return false;

        NodeDomain nd = (NodeDomain)o;

        return checkForAllBinary(nd,EQUALS);
    }

    public DomainKind getDomainKind() {
        return kind;
    }

    public int size() {
        return this.dom.size();
    }

}

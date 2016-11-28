package org.snt.cnetwork.core.domain;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class NodeDomain implements DomainInterface<NodeDomain> {


    private interface DomainAction {
        DomainInterface performOperation(DomainInterface a, DomainInterface b);
    }

    private interface DomainCheck {
        boolean check(DomainInterface a, DomainInterface b);
    }

    private static DomainAction ISECT = (a, b) -> (DomainInterface)a.intersect(b);
    private static DomainAction UNION = (a, b) -> (DomainInterface)a.union(b);
    private static DomainAction MINUS = (a, b) -> (DomainInterface)a.union(b);

    private static DomainCheck SUBSUMPTION = (a, b) -> a.subsumes(b);
    private static DomainCheck EMPTY = (a, b) -> a.isEmpty();
    private static DomainCheck SINGLETON = (a, b) -> a.isSingleton();

    private Map<String, DomainInterface> dom = new HashMap();

    public NodeDomain(NodeDomain other) {
        other.dom.forEach((k,v) ->
            this.dom.put(k, (DomainInterface)v.clone())
        );
    }

    public NodeDomain(DomainInterface ... ds) {
        for(DomainInterface d : ds) {
            dom.put(d.getDomainName(), d);
        }
    }

    public NodeDomain(Set<DomainInterface> ds) {
        for(DomainInterface d : ds) {
            dom.put(d.getDomainName(), d);
        }
    }

    private NodeDomain applyForAll(NodeDomain y, DomainAction d) {
        assert y.dom.size() == dom.size();
        Set<DomainInterface> isects = dom.entrySet().stream().map(e ->
            d.performOperation(e.getValue(),y.dom.get(e))).collect(Collectors
                .toSet());

        return new NodeDomain(isects);
    }

    private boolean checkForAll(NodeDomain y, DomainCheck d) {
        assert y.dom.size() == this.dom.size();
        for(DomainInterface i : dom.values()) {
            DomainInterface yi = y.dom.get(i.getDomainName());
            if(!d.check(i, yi))
                return false;
        }
        return true;
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
    public NodeDomain complement(NodeDomain y) {
        return applyForAll(y, MINUS);
    }

    @Override
    public NodeDomain clone() {
        return new NodeDomain(this);
    }

    @Override
    public boolean subsumes(NodeDomain y) {
        return checkForAll(y, SUBSUMPTION);
    }

    @Override
    public boolean isSingleton() {
        return checkForAll(null, SINGLETON);
    }

    @Override
    public boolean isEmpty() {
        return checkForAll(null, EMPTY);
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


}

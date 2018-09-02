package com.github.hycos.cnetwork.core;

import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkInterface;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkListenerInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Edge;
import com.github.hycos.cnetwork.core.graph.Node;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Set;

public class Enumerator implements ConstraintNetworkListenerInterface<Node, Edge> {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkBuilder.class);


    private Set<Node> nods = new HashSet<>();
    private Set<Edge> edges = new HashSet<>();

    private int eid;
    private int nid;


    public Enumerator(){
        this(0,0);
    }

    public Enumerator(int eid, int nid){
        this.eid = eid;
        this.nid = nid;
    }

    public Enumerator(Enumerator other) {
        this.eid = other.eid;
        this.nid = other.nid;
        this.nods = new HashSet<>(other.nods);
        this.edges = new HashSet<>(other.edges);
    }

    @Override
    public void onNodeCollapse(Node toReplace, Node replacement) throws InconsistencyException {

    }

    @Override
    public void onNodeDelete(Node n) {

    }

    @Override
    public void beforeNodeAdd(Node n) throws InconsistencyException {
        if(n.getId() < 0) {
            n.setId(nid++);
            nods.add(n);
            LOGGER.debug("set node id {}", n.getId());
        }
    }

    @Override
    public void onNodeAdd(Node n, boolean isConstraint) throws InconsistencyException {

    }

    @Override
    public void onConstraintAdd(Node n) throws InconsistencyException {

    }

    @Override
    public void beforeConnectionAdd(Node frst, Node snd, Edge e) {
        if(e.getId() < 0) {
            e.setId(eid++);
            edges.add(e);
            LOGGER.debug("set edge id {}", e.getId());
        }
    }

    @Override
    public void onConnectionAdd(Node frst, Node snd, Edge e) {

    }

    @Override
    public void update(Node n) throws InconsistencyException {

    }

    @Override
    public void attach(Node n) {

    }

    @Override
    public void register(ConstraintNetworkInterface<Node> c) {

    }

    @Override
    public ConstraintNetworkListenerInterface<Node, Edge> clone() {
        return new Enumerator(this);
    }
}

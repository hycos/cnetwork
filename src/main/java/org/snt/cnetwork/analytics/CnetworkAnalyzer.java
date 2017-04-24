package org.snt.cnetwork.analytics;

import org.jgrapht.alg.cycle.PatonCycleBase;
import org.jgrapht.alg.cycle.SzwarcfiterLauerSimpleCycles;
import org.jgrapht.graph.AsUndirectedGraph;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Edge;
import org.snt.cnetwork.core.Node;

import java.util.*;
import java.util.stream.Collectors;


public enum CnetworkAnalyzer {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkAnalyzer.class);


    public List<List<Node>> checkForCycles(ConstraintNetworkBuilder cb) {

        AsUndirectedGraph ag = new AsUndirectedGraph(cb
                .getConstraintNetwork());

        PatonCycleBase pcb  = new PatonCycleBase(ag);

        List<List<Node>> l = pcb.findCycleBase();

        return l;
    }

    public LinkedList<Node> orderTopoligically(ConstraintNetworkBuilder sub) {
        return orderTopoligically(sub.getConstraintNetwork());
    }

    public LinkedList<Node> orderTopoligically(ConstraintNetwork sub) {

        LOGGER.debug("order topologically");
        TopologicalOrderIterator<Node,Edge> it = new TopologicalOrderIterator
                (sub);

        LinkedList<Node> ret = new LinkedList();

        while(it.hasNext()) {
            ret.add(it.next());
        }

        return ret;
    }

    public Set<Node> detectLoopPoints (ConstraintNetworkBuilder cn) {
        return detectLoopPoints(cn.getConstraintNetwork());
    }

    public Set<Node> detectLoopPoints (ConstraintNetwork cn) {

        LOGGER.debug("detect loop pts");

        Set<Node> toOmit = new LinkedHashSet<>();

        ConstraintNetwork cc = cn.clone();

        Set<Edge> dummyedges = new HashSet<>();

        for(Edge e : cc.edgeSet()) {
            if(e.getTarget().isBoolean()) {
                dummyedges.add(new Edge(e.getTarget
                        (), e.getSrcNode(), e.getSequence()));
            }
        }

        dummyedges.stream().forEach(x -> cc.addEdge(x.getSource(), x
                .getDestNode(),x));


        SzwarcfiterLauerSimpleCycles dc = new SzwarcfiterLauerSimpleCycles(cc);

        LOGGER.debug("find simple cycles ");

        List<List<Node>> scycles = dc.findSimpleCycles();

        LOGGER.debug("found scycles ");

        Set<List<Node>> cycles = scycles.stream().filter(l -> l.size() > 2)
                .collect
                (Collectors
                .toSet());


        for(List<Node> cyc : cycles) {

            LinkedList<Node> torder = orderTopoligically(cn.subgraph(cyc));

            //LOGGER.debug("loop {}", torder);

            Node last = torder.getLast();

            if(last.isBoolean())
                toOmit.add(last);

        }

        return toOmit;

    }

//    public Set<Node> detectLoopPoints(ConstraintNetworkBuilder cb) {
//
//        LOGGER.debug("detect loop pts");
//        ConstraintNetwork cn = cb.getConstraintNetwork();
//        Set<Node> toOmit = new LinkedHashSet<>();
//
//        AsUndirectedGraph ag = new AsUndirectedGraph(cn);
//
//        ConnectivityInspector in = new ConnectivityInspector(ag);
//
//
//        List<Set<Node>> rc = in.connectedSets();
//
//        for(Set<Node> r : rc) {
//            LOGGER.debug("RR {}", r);
//        }

        // detect cycles
//        PatonCycleBase pcb = new PatonCycleBase(ag);
//        List<List<Node>> cycles = pcb.findCycleBase();
//
//        Set<Collection<Node>> tcycles = new HashSet<>();
//
//        for(List<Node> cyc : cycles) {
//
//            LinkedList<Node> torder = orderTopoligically(cn.subgraph(cyc));
//
//            LOGGER.debug("loop {}", torder);
//
//            Node last = torder.getLast();
//
//            if(last.isBoolean())
//                toOmit.add(last);
//            tcycles.add(torder);
//        }

//        return toOmit;
//    }



}

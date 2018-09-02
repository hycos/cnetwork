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

package com.github.hycos.cnetwork.analytics;

import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Edge;
import com.github.hycos.cnetwork.core.graph.Node;
import org.jgrapht.alg.cycle.PatonCycleBase;
import org.jgrapht.alg.cycle.SzwarcfiterLauerSimpleCycles;
import org.jgrapht.graph.AsUndirectedGraph;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

        ConstraintNetworkBuilder cnb = cn.clone();


        LOGGER.debug("detect loop pts");

        Set<Node> toOmit = new LinkedHashSet<>();

        Set<Edge> dummyedges = new HashSet<>();

        for(Edge e : cnb.edgeSet()) {
            if(e.getTarget().isBoolean()) {
                dummyedges.add(new Edge(e.getTarget(), e.getSrcNode(), e.getSequence()));
            }
        }

        dummyedges.stream().forEach(x -> {
            try {
                cn.addConnection(x.getSource(), x
                        .getDestNode(),x.getKind(),x.getSequence());
            } catch (InconsistencyException e) {
                assert false;
            }
        });


        SzwarcfiterLauerSimpleCycles dc =
                new SzwarcfiterLauerSimpleCycles(cnb.getConstraintNetwork());

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

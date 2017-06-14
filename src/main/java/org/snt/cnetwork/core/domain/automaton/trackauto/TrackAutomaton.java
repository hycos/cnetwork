package org.snt.cnetwork.core.domain.automaton.trackauto;


import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;
import org.jgrapht.experimental.dag.DirectedAcyclicGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.automaton.SimpleAutomaton;
import org.snt.cnetwork.core.domain.range.NumRange;
import org.snt.cnetwork.utils.DomainUtils;
import org.snt.cnetwork.utils.EscapeUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;


/**
 * Keep track of some operations
 */
public class TrackAutomaton extends DirectedAcyclicGraph<TrackAutomatonNode,
        TrackAutomatonEdge> {

    final static Logger LOGGER = LoggerFactory.getLogger(TrackAutomaton.class);

    private TrackAutomatonNode root = null;
    private int id = 0;

    public TrackAutomaton(String name, String rexp) {
        super(TrackAutomatonEdge.class);
        TrackAutomatonNode mn  = getNewNodeOfKind(TrackAutomatonNode.Kind.LEAF, new RegExp(rexp).toAutomaton(),
                DomainUtils.getApproxLenRange(new SimpleAutomaton(rexp)),name);
        root = mn;
        addVertex(root);
    }


    public TrackAutomaton(String name) {
        this(name, ".*");
    }


    public TrackAutomaton(TrackAutomatonNode.Kind kind, Automaton a, NumRange
            nr){
        super(TrackAutomatonEdge.class);
        root = getNewNodeOfKind(kind, a, nr, "");
        addVertex(root);
    }


    public TrackAutomaton(TrackAutomaton m) {
        super(TrackAutomatonEdge.class);
        Map<TrackAutomatonNode,TrackAutomatonNode> smap = new HashMap<>();

        for(TrackAutomatonNode s : m.vertexSet()) {
            TrackAutomatonNode mn = getNewNodeOfKind(s.getKind(), s
                    .getAutomaton().clone(), s.getRange().clone(),s.getName());
            addVertex(mn);
            smap.put(s, mn);
        }

        for(TrackAutomatonEdge s : m.edgeSet()) {
            try {
                addDagEdge(smap.get(s.getSource()),smap.get(s.getTarget()));
            } catch (CycleFoundException e) {
                assert false;
            }
        }

        root = smap.get(m.root);
        id = m.id;
    }

    private TrackAutomatonNode getNewNodeOfKind(TrackAutomatonNode.Kind kind,
                                                Automaton a, NumRange n,String
                                                        name) {
        return new TrackAutomatonNode(kind, a, n,id++, name);
    }


    public void setName(String name){
        root.setName(name);
    }

    public String getName() {
        return root.getName();
    }

    public TrackAutomatonNode getRoot() {
        return root;
    }

    public void setRoot(TrackAutomatonNode root) {
        this.root = root;
    }


    public TrackAutomaton union(TrackAutomaton a) {
        return performBinOp(TrackAutomatonNode.Kind.UNION, this, a);
    }

    public TrackAutomaton intersect(TrackAutomaton other) {
        return performBinOp(TrackAutomatonNode.Kind.INTERSECTION, this,
         other);
    }

    public TrackAutomaton complement() {
        return performUnaryOp(TrackAutomatonNode.Kind.COMPLEMENT, this);
    }

    public TrackAutomaton minus(TrackAutomaton other) {
        return performBinOp(TrackAutomatonNode.Kind.MINUS, this, other);
    }

    public TrackAutomaton concatenate(TrackAutomaton other) {
        return performBinOp(TrackAutomatonNode.Kind.CONCAT, this, other);
    }


    private static TrackAutomaton performBinOp(TrackAutomatonNode.Kind kind,
                                               TrackAutomaton a1, TrackAutomaton a2) {

        Automaton a1root = a1.getRoot().getAutomaton();
        Automaton a2root = a2.getRoot().getAutomaton();

        NumRange n1root = a1.getRoot().getRange();
        NumRange n2root = a2.getRoot().getRange();

        Automaton aresult = null;
        NumRange nresult = null;

        switch(kind) {
            case UNION:
                aresult = a1root.union(a2root);
                nresult = n1root.union(n2root);
                break;
            case INTERSECTION:
                aresult = a1root.intersection(a2root);
                nresult = n1root.intersect(n2root);
                break;
            case MINUS:
                aresult = a1root.minus(a2root);
                nresult = n1root.minus(n2root);
                break;
            case CONCAT:
                LOGGER.debug("CONCAT OF \n {} \n {} \n", a1.toDot(), a2.toDot());
                aresult = a1root.concatenate(a2root);
                nresult = n1root.numadd(n2root);

                break;
        }

        TrackAutomaton ta = new TrackAutomaton(kind, aresult, nresult);
        ta.addSubGraph(a1);
        ta.addSubGraph(a2);

        LOGGER.debug("== ta");
        LOGGER.debug(ta.toDot());
        LOGGER.debug("== ta");



        return ta;
    }

    private static TrackAutomaton performUnaryOp(TrackAutomatonNode.Kind kind,
                                                 TrackAutomaton a1) {

        Automaton a1root = a1.getRoot().getAutomaton();
        NumRange n1root = a1.getRoot().getRange();

        Automaton aresult = null;
        NumRange nresult = null;

        switch(kind) {
            case COMPLEMENT:
                aresult = a1root.complement();
                nresult = n1root.complement();
                break;
        }

        TrackAutomaton ta = new TrackAutomaton(kind, aresult, nresult);
        ta.addSubGraph(a1);

        return ta;
    }



    private void addSubGraph(TrackAutomaton other) {

        Map<TrackAutomatonNode,TrackAutomatonNode> smap = new HashMap<>();

        for(TrackAutomatonNode s : other.getOrderedVertices()) {
            TrackAutomatonNode mn = getNewNodeOfKind(s.getKind(),s
                    .getAutomaton().clone(),s.getRange().clone(),s.getName());
            smap.put(s, mn);
        }

        for(TrackAutomatonNode n : smap.values()) {
            LOGGER.debug("add v {}", n.getId());
            addVertex(n);
        }

        for(TrackAutomatonEdge s : other.edgeSet()) {
            try {
                addDagEdge(smap.get(s.getSource()),smap.get(s.getTarget()));
            } catch (CycleFoundException e) {
                assert false;
            }
        }

        try {
            addDagEdge(root, smap.get(other.root));
        } catch (CycleFoundException e) {
            assert false;
        }

    }


    private Set<TrackAutomatonNode> getOrderedVertices() {
        Set<TrackAutomatonNode> tset = new TreeSet<>();
        tset.addAll(vertexSet());
        return tset;
    }



    public String toDot() {

        StringBuilder sb = new StringBuilder();
        sb.append("digraph {\n" +
                "\trankdir=TB;\n");

        sb.append("\tnode [fontname=Helvetica,fontsize=11];\n");
        sb.append("\tedge [fontname=Helvetica,fontsize=10];\n");


        for (TrackAutomatonNode n : getOrderedVertices()) {
            String shape = "";
            String color = "";

            String name = n.getName();


            sb.append("\tn" + n.getId() + " [label=\"" + n.getKind().toString
                    () + "\\n" + EscapeUtils.escapeSpecialCharacters(n
                            .getRange().toString()) + "\\n" +
                    name  + "\"," + "shape=\"" + shape +
                    "\", color=\"" +
                    color + "\"];\n");
        }


        for (TrackAutomatonEdge e : this.edgeSet())  {

            TrackAutomatonNode src = e.getSource();
            TrackAutomatonNode dst = e.getTarget();


            sb.append("\tn" + src.getId() + " -> n" + dst.getId() +"\n");
        }
        sb.append("}\n");

        return sb.toString();
    }



}

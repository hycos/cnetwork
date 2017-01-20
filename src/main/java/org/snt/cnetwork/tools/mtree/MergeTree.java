package org.snt.cnetwork.tools.mtree;


import org.apache.commons.collections.map.HashedMap;
import org.jgrapht.graph.DirectedPseudograph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;


public class MergeTree<T extends Element> extends
        DirectedPseudograph<EquiClass<T>, EquiEdge<T>> {

    final static Logger LOGGER = LoggerFactory.getLogger(MergeTree.class);

    private EquiClass top = new EquiClass.Top();

    private Map<EquiClass<T>, EquiClass<T>> nlookup = new HashedMap();

    public MergeTree() {
        super(new EdgeFact());
    }


    public boolean addVertex(T ... elems) {
        return addVertex(new EquiClass<T>(Arrays.asList(elems)));
    }

    @Override
    public boolean addVertex(EquiClass<T> n) {

        LinkedList<EquiClass<T>> worklist = new LinkedList<>();
        Set<EquiEdge> edges = new LinkedHashSet<>();

        worklist.add(n);

        while (!worklist.isEmpty()) {

            EquiClass<T> parent = worklist.removeFirst();
            int idx = 0;
            for(EquiClass<T> s : parent.split()) {
                if(parent.isSingleton()) {
                    addParEdge(parent, s, idx ++);
                } else {
                    addSubEdge(parent, s);
                }
                worklist.addLast(s);
            }
        }

        return true;

    }


    public EquiClass<T> checkAndGet(EquiClass<T> v) {
        if(!nlookup.containsKey(v)) {
            nlookup.put(v,v);
            super.addVertex(v);
        }
        return nlookup.get(v);
    }

    public void addParEdge(EquiClass<T> src, EquiClass<T> dst, int idx) {
        addEdge(src,dst, EquiEdge.Kind.PAR, idx);
    }


    public void addSubEdge(EquiClass<T> src, EquiClass<T> dst) {
        addEdge(src,dst, EquiEdge.Kind.SUB, -1);
    }

    public void addEdge(EquiClass<T> src, EquiClass<T> dst, EquiEdge.Kind kind,
                        int
            idx) {
        src = checkAndGet(src);
        dst = checkAndGet(dst);

        EquiEdge e = new EquiEdge(src, dst, kind, idx);

        addEdge(e.getSource(),e.getTarget(),e);
    }

    public String toDot() {

        StringBuilder sb = new StringBuilder();

        sb.append("digraph {\n" +
                "\trankdir=TB;\n");

        sb.append("\tnode [fontname=Helvetica,fontsize=11];\n");
        sb.append("\tedge [fontname=Helvetica,fontsize=10];\n");

        String shape = "";
        String label = "";
        String color = "black";

        for (EquiClass<T> n : this.vertexSet()) {
            String kind = "";

            shape = "box";
            label = "label";


            sb.append("\tn" + n.getId() + " [color=" + color + ",shape=\"" +
                    shape + "\"," + label + "=\"" + n.getDotLabel() + "\"];\n");
        }

        String option = "";
        String ecolor = "black";
        String par = "";

        LOGGER.debug("eset {}", edgeSet().size());

        for (EquiEdge e : this.edgeSet()) {
            EquiClass<T> src = e.getSource();
            EquiClass<T> dest = e.getTarget();

            assert (outgoingEdgesOf(src).contains(e));
            assert (incomingEdgesOf(dest).contains(e));

            assert (src != null);
            assert (dest != null);

            ecolor = "black";
            par = "";

            if (e.getKind() == EquiEdge.Kind.PAR) {
                ecolor = "blue";
                par = "" + e.getSequence();
            } else if (e.getKind() == EquiEdge.Kind.SUB) {
                ecolor = "orange";
            }


            sb.append("\tn" + src.getId() + " -> n" + dest.getId() +
                    "[color=\"" + ecolor + "\",label=\"" + par.trim() + "\"];\n");

        }


        sb.append("}");

        return sb.toString();
    }

}

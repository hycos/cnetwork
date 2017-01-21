package org.snt.cnetwork.core.mergelattice;


import org.apache.commons.collections.map.HashedMap;
import org.jgrapht.experimental.dag.DirectedAcyclicGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;
import java.util.stream.Collectors;


public class MergeLattice<T> extends
        DirectedAcyclicGraph<EquiClass, EquiEdge> {

    final static Logger LOGGER = LoggerFactory.getLogger(MergeLattice.class);

    private EquiClass top = new EquiClass.Top();
    private EquiClass bottom = new EquiClass.Bottom();
    private EquiEdge init = new EquiEdge(top,bottom, EquiEdge.Kind.SUB,-1);

    private EquiClassFact elementFact = null;

    private Map<EquiClass, EquiClass> nlookup = new HashedMap();

    public MergeLattice(EquiClassFact<T> elementFact) {
        super(new EdgeFact());
        super.addVertex(top);
        super.addVertex(bottom);
        super.addEdge(top,bottom,init);
        this.elementFact = elementFact;
    }




    private EquiClass findParent(EquiClass e) {

        EquiClass cursor = bottom;

        while(!cursor.subsumes(e)) {

            try {
                cursor = incomingEdgesOf(cursor).stream()
                        .map(EquiEdge::getSource).
                                filter(s -> s.hasOverlap(e)).findFirst().get();
            } catch (NoSuchElementException ex) {
                // if there is no overlap, we know that T has to be the parent
                return top;
            }
        }

        return cursor;
    }

    private void split(EquiClass n) {


        LinkedList<EquiClass> worklist = new LinkedList<>();

        worklist.add(n);

        while (!worklist.isEmpty()) {

            EquiClass parent = worklist.removeFirst();


            LOGGER.debug("parent {}", parent);
            int idx = 0;
            for(EquiClass s : parent.split()) {
                if(parent.isSingleton()) {
                    addParEdge(parent, s, ++idx);
                } else {
                    LOGGER.debug("parent {} no single", parent);
                    addSubEdge(parent, s);
                }

                if(!nlookup.containsKey(s))
                    worklist.addLast(s);
            }
        }
    }

    private void removeTopLinks(EquiClass n) {

        if(!containsVertex(n))
            return;

        Set<EquiEdge> topcon = null;
        try {
            topcon = incomingEdgesOfKind(n, EquiEdge.Kind.SUB)
                    .stream().filter(s -> s.getSource().equals(top)).collect(Collectors.toSet());
        } catch (NoSuchElementException e) {
            return;
        }
        super.removeAllEdges(topcon);
    }

    public void addEquiClasses(T []... toadd) {
        addEquiClasses(elementFact.create(toadd));
    }

    public void addEquiClasses(EquiClass [] toadd) {
        addEquiClasses(Arrays.asList(toadd));
    }

    public void addEquiClasses(Collection<EquiClass> toadd) {
        toadd.forEach(e->addEquiClass(e));
    }

    public void addEquiClass(EquiClass n) {
        LOGGER.debug("add equivalence class {}", n);

        if(nlookup.containsKey(n) || n.isEmpty())
            return;

        addSubEdge(findParent(n), n);

        // split the equiclass and
        split(n);
        //merge();

        //if(edgeSet().contains(init)) {
        //    removeEdge(init);
        //    LOGGER.debug("rm init edge {}", init);
        //}
    }



    public Set<EquiEdge> incomingEdgesOfKind(EquiClass e, EquiEdge.Kind
            kind) {

        assert vertexSet().contains(e);

        return incomingEdgesOf(e).stream().filter(ne -> ne.getKind() == kind)
                .collect(Collectors.toSet());
    }

    public Set<EquiEdge> outgoingEdgesOfKind(EquiClass e, EquiEdge.Kind
            kind) {
        assert vertexSet().contains(e);
        return outgoingEdgesOf(e).stream().filter(ne -> ne.getKind() == kind)
                .collect(Collectors.toSet());
    }

    public Set<EquiEdge> incomingEdges(EquiClass e, EquiEdge.Kind
            kind ) {
        return incomingEdgesOf(e).stream().collect(Collectors.toSet());
    }


    @Override
    public boolean addVertex(EquiClass e) {
        throw new NotImplementedException();
    }


    @Override
    public boolean removeVertex(EquiClass e) {
        throw new NotImplementedException();
    }

    @Override
    public EquiEdge addEdge(EquiClass src, EquiClass dst) {
        throw new NotImplementedException();
    }

    @Override
    public boolean addEdge(EquiClass src, EquiClass dst, EquiEdge e) {
        throw new NotImplementedException();
    }

    public EquiClass checkAndGet(EquiClass v) {
        if(!nlookup.containsKey(v)) {
            nlookup.put(v,v);
            super.addVertex(v);
        }
        return nlookup.get(v);
    }

    public void addParEdge(EquiClass src, EquiClass dst, int idx) {
        LOGGER.debug("add par edge {} -> {}", src, dst);
        addEdge(src,dst, EquiEdge.Kind.PAR, idx);

        linkToTop(dst);

        if(dst.isSingleton())
            linkToBottom(dst);
    }


    public void addSubEdge(EquiClass src, EquiClass dst) {
        LOGGER.debug("add sub edge {} -> {}", src, dst);

        // if destination object is linked to top, we can remove it
        // because we know that it already has another parent
        removeTopLinks(dst);
        addEdge(src,dst, EquiEdge.Kind.SUB, -1);

        if(dst.isSingleton())
            linkToBottom(dst);
    }


    private boolean hasIncomingEdgesOfKind(EquiClass n, EquiEdge.Kind kind) {
        Set<EquiEdge> in = incomingEdgesOfKind(n, kind);
        return in != null && in.size() >0;
    }

    private boolean hasOutgoingEdgesOfKind(EquiClass n, EquiEdge.Kind kind) {
        Set<EquiEdge> out = outgoingEdgesOfKind(n, kind);
        return out != null && out.size() >0;
    }

    private EquiClass getSingleSuperclass(EquiClass child) {
        assert incomingEdgesOfKind(child, EquiEdge.Kind.SUB).size() == 1;

        return incomingEdgesOfKind(child, EquiEdge.Kind.SUB).iterator().next
                ().getSource();

    }

    private EquiClass nextSubMergePt(){
        try {
            return vertexSet().stream()
                    .filter(v ->
                        incomingEdgesOfKind(v, EquiEdge.Kind.SUB).size() > 1
                                && !hasIncomingEdgesOfKind(v, EquiEdge.Kind.PAR)
                                && !v.equals(bottom)
                    ).findFirst()
                    .get();
        } catch (NoSuchElementException | NullPointerException e) {
            return null;
        }
    }


    private Set<EquiClass> getFoosToMerge(){
        Set<EquiClass> foosToMerge = new HashSet<>();
        try {
            // get functions first
            Set<EquiClass> foos = vertexSet().stream().filter(v ->
                    isFunction(v)).collect(Collectors.toSet());

            // compute cardinality
            for(EquiClass f : foos) {

                for(EquiClass p : getParamsFor(f)) {

                    EquiClass sc = getSingleSuperclass(p);

                    LOGGER.debug("parsub {} CC {} ", p, sc);
                    LOGGER.debug("CC " + sc.getCardinality
                            ());
                }

                int card = getParamsFor(f).stream()
                        .map(p -> getSingleSuperclass(p).getCardinality())
                        .reduce((x,y) -> x * y).get();

                LOGGER.debug("foo card {} vs sub card {}", f.getCardinality()
                        , card);
                // new function signatures could be inferred for f
                if(card != f.getCardinality()) {
                    foosToMerge.add(f);
                }
            }

        } catch (NoSuchElementException | NullPointerException e) {
            return foosToMerge;
        }
        return foosToMerge;
    }

    private boolean isFunction(EquiClass e) {
        return hasOutgoingEdgesOfKind(e, EquiEdge.Kind.PAR);
    }

    private boolean isParam(EquiClass e) {
        return hasIncomingEdgesOfKind(e, EquiEdge.Kind.PAR);
    }

    private boolean isDirectSuccessorOfTop(EquiClass e) {
        return incomingEdgesOfKind(e, EquiEdge.Kind.SUB).stream().filter(
                k -> k.getSource().equals(top)
        ).count() > 0;
    }

    private void mergeSub() {
        LOGGER.debug("merge subclasses");
        EquiClass nxt = null;

        while((nxt = nextSubMergePt()) != null) {

            Set<EquiEdge> in = incomingEdgesOfKind(nxt, EquiEdge.Kind.SUB);

            // merge super sets together

            Set<EquiClass> tomerge = in.stream().map(EquiEdge::getSource)
                    .collect(Collectors.toSet());

            Set mergedElements = tomerge.stream().map(EquiClass::getElements).flatMap(x ->
                    x.stream()).collect(Collectors.toSet());

            EquiClass merge = new EquiClass(mergedElements);

            replace(tomerge, merge);
        }
    }

    /**private void mergeFoos() {
        LOGGER.debug("merge parameters");

        Set<EquiClass> toAdd = new HashSet<EquiClass>();
        for(EquiClass foo : getFoosToMerge()){
            LOGGER.debug("handle foo {}" , foo);
            toAdd.add(handleFoo(foo));
        }

        toAdd.forEach(e -> split(e));
    }**/

    private Set<EquiClass> getParamsFor(EquiClass foo) {
        return outgoingEdgesOfKind(foo, EquiEdge.Kind.PAR).stream().map
                (EquiEdge::getTarget).collect(Collectors.toSet());
    }

    /**private EquiClass handleFoo(EquiClass foo) {

        EquiClass fequi = new EquiClass();

        for(EquiEdge pedge : outgoingEdgesOfKind(foo,EquiEdge.Kind.PAR)) {

            EquiClass par = pedge.getTarget();

            EquiClass psup = getSingleSuperclass(par);

            if(psup.equals(top))
                continue;


            assert foo.getCardinality() == 1;

            Element ele = foo.getElements().iterator().next();

            // split the function into pieces
            Collection<? extends Element> split = ele.split(0);

            Element [] segments = split.toArray(new Element[split.size()]);

            int pidx = pedge.getSequence();

            assert pidx < segments.length && pidx >= 1;

            for (Element alias : psup.getElements()) {
                segments[pidx] = alias;
            }

            LOGGER.debug("add foo equi class {}", fequi);
            System.exit(-1);

            Element nele  = elementFact.join(segments);

            fequi.add(nele);
            LOGGER.debug("add foo equi class {}", fequi);

        }

        return fequi;
    }**/


    private void merge() {
        mergeSub();
        //mergeFoos();
    }

    private void replace(Set<EquiClass> toReplace, EquiClass replacement) {

        Set<EquiEdge> edges = toReplace.stream().map(v -> outgoingEdgesOf(v))
                .flatMap(x -> x.stream())
                .filter(e -> !replacement.equals(e.getTarget()))
                .map(e -> new EquiEdge(replacement,e
                .getTarget(),e.getKind(),e
                .getSequence())).collect(Collectors.toSet());

        edges.addAll(toReplace.stream().map(v -> incomingEdgesOf(v))
                .flatMap(x -> x.stream())
                .filter(e -> !replacement.equals(e.getSource()))
                .map(e -> new EquiEdge(e.getSource
                        (),replacement,e.getKind(),e
                        .getSequence())).collect(Collectors.toSet()));

        removeEquiClasses(toReplace);

        addEdges(edges);
        split(replacement);
    }



    private void removeEquiClasses(Collection<EquiClass> v) {
        v.forEach( e -> removeEquiClass(e));
    }

    private void removeEquiClass(EquiClass v) {
        super.removeVertex(v);
        nlookup.remove(v);
    }

    private void addEdges(Set<EquiEdge> edges) {
        edges.forEach(e -> addEdge(e));
    }

    private void addEdge(EquiEdge e) {
        addEdge(e.getSource(), e.getTarget(), e.getKind(), e.getSequence());
    }

    private void addEdge(EquiClass src, EquiClass dst, EquiEdge.Kind kind,
                        int
            idx) {
        src = checkAndGet(src);
        dst = checkAndGet(dst);

        EquiEdge e = new EquiEdge(src, dst, kind, idx);
        LOGGER.debug("add edge {} -> {}", src, dst);

        super.addEdge(e.getSource(),e.getTarget(),e);
    }

    private void linkToTop(EquiClass dst) {
        if(inSubDegreeOf(dst) == 0 && !dst.equals(top)) {
            addSubEdge(top, dst);
        }
    }

    private void linkToBottom(EquiClass dst) {
        if(outSubDegreeOf(dst) == 0 && !dst.equals(bottom)) {
            addSubEdge(dst,bottom);
        }
    }


    private int inSubDegreeOf(EquiClass n) {
        return incomingEdgesOfKind(n, EquiEdge.Kind.SUB).size();
    }

    private int outSubDegreeOf(EquiClass n) {
        return outgoingEdgesOfKind(n, EquiEdge.Kind.SUB).size();
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

        for (EquiClass n : this.vertexSet()) {
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
            EquiClass src = e.getSource();
            EquiClass dest = e.getTarget();

            assert outgoingEdgesOf(src).contains(e);
            assert incomingEdgesOf(dest).contains(e);

            assert src != null;
            assert dest != null;

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

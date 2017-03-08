package org.snt.cnetwork.core.mergelattice;


import org.jgrapht.graph.DirectedMultigraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;
import org.snt.cnetwork.utils.BiMap;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class EufLattice<T> extends
        DirectedMultigraph<EquiClass, EquiEdge> implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(EufLattice.class);

    private EquiClass top = new EquiClass.Top();
    private EquiClass bottom = new EquiClass.Bottom();
    private EquiEdge init = new EquiEdge(top, bottom, EquiEdge.Kind.SUB, -1);

    private EquiClassFact elementFact = null;

    private BiMap<String, EquiClass> ecache = new BiMap<>();

    public void setEquiClassFact(EquiClassFact<T> elementFact) {
        this.elementFact = elementFact;
    }

    public EufLattice(EquiClassFact<T> elementFact) {
        super(new EdgeFact());
        super.addVertex(top);
        super.addVertex(bottom);
        super.addEdge(top, bottom, init);
        this.elementFact = elementFact;
    }


    public EufLattice(EufLattice<T> other, EquiClassFact<T> elementFact) {
        this(elementFact);

        for (EquiClass e : other.vertexSet()) {
            try {
                addEquiClass(e);
            } catch (EUFInconsistencyException e1) {
                assert false;
            }
        }

        for (EquiEdge e : other.edgeSet()) {
            addEdge(e);
        }

    }

    /**
     * API
     **/
    public void addInequialityConstraint(T... e) throws EUFInconsistencyException {

        //LOGGER.debug("add inequality constraint");

        assert e.length == 2;


        if (!elementFact.hasEquiClassFor(e[0])) {
            LOGGER.debug("ADD 0 {}", e[0]);
            addEquiClass(e[0]);
        }

        if (!elementFact.hasEquiClassFor(e[1])) {
            LOGGER.debug("ADD 1 {}", e[1]);
            addEquiClass(e[1]);
        }
        EquiClass[] ec = new EquiClass[2];

        try {
            ec[0] = elementFact.getEquiClassFor(e[0]);
            ec[1] = elementFact.getEquiClassFor(e[1]);
        } catch (MissingItemException e1) {
            assert false;
        }

        assert ec.length == 2;

        EquiClass fst = getCoveringEquiClass(ec[0]);
        EquiClass snd = getCoveringEquiClass(ec[1]);


        LOGGER.debug("fst {} par {}", ec[0].getDotLabel(), fst.getDotLabel());
        LOGGER.debug("snd {} par {}", ec[1].getDotLabel(), snd.getDotLabel());


        // cannot create an ineq edge where source and dest are pointing to the
        // same equi class
        if (fst.equals(snd)) {
            throw new EUFInconsistencyException("Inconsistency detected " +
                    "between " + fst + " " + snd);
        }


        fst = fst.equals(top) ? ec[0] : fst;
        snd = snd.equals(top) ? ec[1] : snd;


        addIneqEdge(fst, snd);
        addIneqEdge(snd, fst);
    }

    private EquiClass addEquiClass(T toadd)
            throws EUFInconsistencyException {
        return addEquiClass(elementFact.createEquiClass(toadd));
    }

    public EquiClass addEquiClass(T... toadd)
            throws EUFInconsistencyException {
        assert elementFact != null;
        return addEquiClass(elementFact.createEquiClasses(toadd));
    }

    public EquiClass join(T... e) throws MissingItemException {
        return join(elementFact.getEquiClassesFor(e));
    }


    public EquiClass getBottom() {
        return this.bottom;
    }

    public EquiClass getTop() {
        return this.top;
    }
    /**
     * API
     **/


    private EquiClass union(Collection<EquiClass> e) {
        assert !e.isEmpty();
        return e.stream().reduce(EquiClass::union).get();
    }

    private EquiClass intersection(Collection<EquiClass> e) {
        assert !e.isEmpty();
        return e.stream().reduce(EquiClass::intersection).get();
    }

    private void insert(final EquiClass e) throws EUFInconsistencyException {

        if(isAlreadySubsumed(e))
            return;

        EquiClass mo = e;
        LOGGER.debug("insert {}", e.getDotLabel());


        // 1. geneerate max overlap
        try {
            mo = mo.union(getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB).
                    stream().filter(n -> n.hasOverlap(e)
            ).reduce(EquiClass::union).get());
        } catch (NoSuchElementException ex) {}

        // 2. cleanup lattice
        EquiClass finalMo = mo;

        Stream<EquiClass> ostream =
                getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB)
                        .stream().filter(n -> finalMo.subsumes(n) &&
                        !finalMo.equals(n));

        Set<EquiClass> sub = ostream.collect(Collectors.toSet());


        // 3. check consistency - there cannot be an ineq edge pointing from
        // and to the same equi class
        boolean inconsistent = edgeSet().stream()
                .filter(t -> t.getKind() == EquiEdge.Kind.INEQ)
                .filter(t -> finalMo.hasOverlap(t.getSource()))
                .filter(t -> finalMo.hasOverlap(t.getTarget())).count() > 0;

        if (inconsistent) {
            throw new EUFInconsistencyException("Equi class " + mo + "contradicts " +
                    "give" +
                    "n constraints");
        }


        if(sub.isEmpty()) {
            linkToTop(mo);
        } else {
            replace(sub, mo);
        }

        // split the equi class in order to learn interesting facts
        // by analyzing nested equi classes
        split(mo);
    }

    private void checkParamBackwards(EquiClass c) {

        Set<EquiEdge> isplit = incomingEdgesOfKind(c, EquiEdge.Kind.SPLIT);



    }


    private EquiClass findSubsumptionPoint(EquiClass e) {

        EquiClass cursor = bottom;

        while (!cursor.subsumes(e)) {

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

    public EquiClass getCoveringEquiClass(EquiClass o) {
        try {
            return getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB).stream()
                    .filter(x -> x.hasOverlap(o)).findFirst().get();
        } catch (NoSuchElementException e) {
            return o;
        }
    }


    private void split(EquiClass n) {

        LinkedList<EquiClass> worklist = new LinkedList<>();
        LOGGER.debug("split {}", n.getDotLabel());


        // n is a non-nested class
        if (!n.isNested()){

            Set<EquiClass> nsub = n.split().stream().filter(e -> e.isNested())
                    .collect(Collectors.toSet());

            // no nested elements contained
            if(nsub.isEmpty())
                return;


            nsub.forEach(s -> addSubEdge(n, s));

            worklist.addAll(nsub);


        } else {
            worklist.add(n);
        }


        while (!worklist.isEmpty()) {
            EquiClass next = worklist.pop();
            worklist.addAll(handleNestedElement(next));
        }
        LOGGER.debug(toDot());
    }


    private Set<EquiClass> handleNestedElement(EquiClass parent) {
        int x = 0;

        LOGGER.debug("handle nested element {}", parent.getDotLabel());
        Set<EquiClass> ret = new HashSet<>();

        assert parent.isNested();

        // this is all the parameters
        for(EquiClass n : parent.split()) {
            EquiClass alias = getCoveringEquiClass(n);

            LOGGER.debug("Alias for {} is {}", n.getDotLabel(), alias.getDotLabel());

            addSplitEdge(parent, alias, ++x);

            if(n.isNested()) {
                ret.add(n);
            }

        }
        return ret;
    }


    public EquiClass addEquiClass(Collection<EquiClass> toadd) throws
            EUFInconsistencyException {

        EquiClass eq = union(toadd);

        for(EquiClass e : toadd) {
            //LOGGER.debug(">> {}", e.getDotLabel());
            addEquiClass(e);
        }
        return findSubsumptionPoint(eq);
    }




    public void addEquiClass(EquiClass n)
            throws EUFInconsistencyException {

        LOGGER.debug("(+) add equi class {}", n.getDotLabel());


        if (isAlreadySubsumed(n) || n.isEmpty()) {
            LOGGER.debug("already subsumed");
            return;
        }

        LOGGER.debug("find max ov for {}", n.getDotLabel());
        insert(n);

        if (edgeSet().contains(init)) {
            removeEdge(init);
            //LOGGER.debug("rm init edge {}", init);
        }

    }


    public Set<EquiEdge> outgoingEdgesOfKind(EquiClass e, EquiEdge.Kind
            kind) {
        Set<EquiEdge> ret = new HashSet<>();
        if (vertexSet().contains(e)) {
            ret.addAll(outgoingEdgesOf(e).stream().filter(ne -> ne.getKind() ==
                    kind)
                    .collect(Collectors.toSet()));
        }
        return ret;
    }

    public Set<EquiEdge> incomingEdgesOfKind(EquiClass e, EquiEdge.Kind
            kind) {
        Set<EquiEdge> ret = new HashSet<>();
        if (vertexSet().contains(e)) {
            ret.addAll(incomingEdgesOf(e).stream().filter(ne -> ne.getKind() ==
                    kind)
                    .collect(Collectors.toSet()));
        }
        return ret;
    }

    public Set<EquiClass> getConnectedOutNodesOfKind(EquiClass e, EquiEdge
            .Kind kind) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        ret.addAll(outgoingEdgesOfKind(e, kind).stream().map(EquiEdge::getTarget)
                .collect(Collectors.toSet()));
        return ret;
    }

    public Set<EquiClass> getConnectedInNodesOfKind(EquiClass e, EquiEdge
            .Kind kind) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        ret.addAll(incomingEdgesOfKind(e, kind).stream().map(EquiEdge::getSource)
                .collect
                        (Collectors.toSet()));
        return ret;
    }


    public Set<EquiClass> getConnectedOutNodes(EquiClass e) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        ret.addAll(outgoingEdgesOf(e).stream().map(EquiEdge::getTarget)
                .collect(Collectors.toSet()));
        return ret;
    }

    public Set<EquiClass> getConnectedInNodesOf(EquiClass e) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        ret.addAll(incomingEdgesOf(e).stream().map(EquiEdge::getSource)
                .collect(Collectors.toSet()));
        return ret;
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
        if (!vertexSet().contains(v)) {
            super.addVertex(v);
        }
        return v;
    }

    public void addSplitEdge(EquiClass src, EquiClass dst, int idx) {
        LOGGER.debug("add split edge {} -> {}", src.getDotLabel(), dst
                .getDotLabel());

        assert src.isNested();


        addEdge(src, dst, EquiEdge.Kind.SPLIT, idx);

        if(inSubDegreeOf(src) == 0)
            linkToTop(src);

        if (inSubDegreeOf(dst) == 0)
            linkToTop(dst);

        if (outSubDegreeOf(dst) == 0)
            linkToBottom(dst);
    }


    public void addSubEdge(EquiClass src, EquiClass dst) {
        LOGGER.debug("add sub edge {} -> {}", src.getDotLabel(), dst.getDotLabel());

        //assert  src.subsumes(dst);

        // no link to bottom require anymore
        removeEdge(src,bottom);

        addEdge(src, dst, EquiEdge.Kind.SUB, -1);


        if (inSubDegreeOf(src) == 0)
            linkToTop(src);

        if (outSubDegreeOf(dst) == 0)
            linkToBottom(dst);
    }

    public void addIneqEdge(EquiClass src, EquiClass dst) {
        LOGGER.debug("add sub edge {} -> {}", src, dst);
        addEdge(src, dst, EquiEdge.Kind.INEQ, -1);
    }


    private boolean hasIncomingEdgesOfKind(EquiClass n, EquiEdge.Kind kind) {
        Set<EquiEdge> in = incomingEdgesOfKind(n, kind);
        return in != null && in.size() > 0;
    }

    private boolean hasOutgoingEdgesOfKind(EquiClass n, EquiEdge.Kind kind) {
        Set<EquiEdge> out = outgoingEdgesOfKind(n, kind);
        return out != null && out.size() > 0;
    }


    private EquiClass join(EquiClass... e) {

        Objects.requireNonNull(e, "join cannot be called with a null " +
                "parameter");

        EquiClass u = union(Arrays.asList(e));

        return findSubsumptionPoint(u);
    }

    // given a nested equiclass infer the equivalent based on parameter
    // equivalence
    public EquiClass inferEquiClassFor(EquiClass e) {
        assert e.isNested();
        assert e.getElements().size() == 1;

        LOGGER.debug("infer equivalence class for {}", e.getDotLabel());

        final String anno = e.getElements().iterator().next().getAnnotation();


        // if there exist an alias, it has to be among this elems
        Set<EquiClass> srccrit = vertexSet().stream()
                .filter(v -> v.isNested())
                .filter(v -> v.getElements().iterator().next().getAnnotation
                        ().equals(anno)).collect(Collectors.toSet());

        if(srccrit.isEmpty())
            return bottom;

        LOGGER.debug("srccrit {}", srccrit);
        assert e.getElements().size() == 1;

        LOGGER.debug("card {}", e.getCardinality());

        LOGGER.debug("spl {}", e.split().size());

        LOGGER.debug("CL {}", e.getClass());
        // get the paramters
        Collection<EquiClass> ec = e.split();

        assert ec.size() > 0;

        LOGGER.debug("split is {}", ec);

        Set<EquiClass> tcrit = new HashSet<>();

        // get
        int pidx = 0;
        for(EquiClass eq : ec) {
            EquiClass cov = getCoveringEquiClass(eq);


            LOGGER.debug("covering {} for {}", cov.getDotLabel(), eq.getDotLabel
                    ());


            // we can directly stop - param does not have aliases yet
            if(!vertexSet().contains(cov)) {
                LOGGER.debug("not there yet");
                return e;
            }

            assert vertexSet().contains(cov);

            final int idx = pidx ++;
            Set<EquiClass> inc = incomingEdgesOf(cov).stream()
                    .filter(t -> t.getKind() == EquiEdge.Kind.SPLIT)
                    .filter(t -> t.getSequence() == idx)
                    .filter(t -> !t.getSource().equals(top))
                    .map(t -> t.getSource()).collect(Collectors.toSet());


            if(idx == 1) {
                tcrit.addAll(inc);
            } else {
                tcrit.retainAll(inc);
            }
            pidx ++;
        }

        if(srccrit.isEmpty() || tcrit.isEmpty()) {
            return bottom;
        }

        Set<EquiClass> chop = fwslice(srccrit);
        chop.retainAll(bwslice(tcrit));

        try {
            chop = chop.stream().filter(v -> v.isNested())
                    .filter(v -> v.getElements().iterator().next().getAnnotation
                            ().equals(anno)).collect(Collectors.toSet());
        } catch(NoSuchElementException x) {
            return bottom;
        }


        if(chop.size() == 1) {
            return chop.iterator().next();
        } else {
            LOGGER.debug("chop {}", chop);
            LOGGER.debug("chop size {}", chop.size());
            assert chop.isEmpty();
            return bottom;
        }

    }

    private Set<EquiClass> bwslice(Collection<EquiClass> crit) {
        LOGGER.debug("bw slice");
        LinkedList<EquiClass> wlist = new LinkedList<>();
        wlist.addAll(crit);
        Set<EquiClass> slice = new HashSet<>();
        while(!wlist.isEmpty()) {
            EquiClass ec = wlist.pop();

            if(slice.contains(ec))
                continue;

            slice.add(ec);
            if(inDegreeOf(ec) > 0){
                wlist.addAll(getConnectedInNodesOfKind(ec, EquiEdge.Kind.SUB));
            }
        }
        return slice;
    }

    private Set<EquiClass> fwslice(Collection<EquiClass> crit) {
        LOGGER.debug("fw slice");
        LinkedList<EquiClass> wlist = new LinkedList<>();
        wlist.addAll(crit);
        Set<EquiClass> slice = new HashSet<>();

        while(!wlist.isEmpty()) {
            EquiClass ec = wlist.pop();

            if(slice.contains(ec))
                continue;

            slice.add(ec);
            if(outDegreeOf(ec) > 0){
                wlist.addAll(getConnectedInNodesOfKind(ec, EquiEdge.Kind.SUB));
            }
        }

        return slice;
    }

    private boolean isAlreadySubsumed(EquiClass n) {

        return outgoingEdgesOfKind(top, EquiEdge.Kind.SUB).stream().map
                (EquiEdge::getTarget).filter(t -> t.subsumes(n)).count() > 0;
    }


    private void replace(Set<EquiClass> toReplace, EquiClass replacement) {

        if (toReplace.isEmpty())
            return;


        Set<EquiEdge> edges = new HashSet<>();

        Set<EquiEdge> out = toReplace.stream().map(v -> outgoingEdgesOf(v))
                .flatMap(x -> x.stream()).collect(Collectors.toSet());


        Set<EquiEdge> in = toReplace.stream().map(v -> incomingEdgesOf(v))
                .flatMap(x -> x.stream()).collect(Collectors.toSet());


        removeEquiClasses(toReplace);

        in.stream().forEach( e ->
                addEdge(new EquiEdge(e.getSource(), replacement, e.getKind(), e
                        .getSequence())
        ));

        out.stream().filter(e -> e.getKind() == EquiEdge.Kind.SUB).forEach(e ->
                addEdge(new EquiEdge(replacement, e.getTarget(), e.getKind(), e
                        .getSequence())
                ));





        LOGGER.debug("toreplace {}", toReplace);
        LOGGER.debug("rpl {}", replacement);

        removeEquiClasses(toReplace);

        addEdges(edges);
        split(replacement);
    }


    private void removeEquiClasses(Collection<EquiClass> v) {
        v.forEach(e -> removeEquiClass(e));
    }

    private void removeEquiClass(EquiClass v) {
        LOGGER.debug("remove vertex {}", v);
        super.removeVertex(v);
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
        //LOGGER.debug("add edge {} -> {}", src, dst);

        super.addEdge(e.getSource(), e.getTarget(), e);
    }

    private void linkToTop(EquiClass dst) {
        if (inSubDegreeOf(dst) == 0 && !dst.equals(top)) {
            addSubEdge(top, dst);
        }
    }

    private void linkToBottom(EquiClass dst) {
        if (outSubDegreeOf(dst) == 0 && !dst.equals(bottom)) {
            addSubEdge(dst, bottom);
        }
    }


    private int inSubDegreeOf(EquiClass n) {
        return incomingEdgesOfKind(n, EquiEdge.Kind.SUB).size();
    }

    private int outSubDegreeOf(EquiClass n) {
        return outgoingEdgesOfKind(n, EquiEdge.Kind.SUB).size();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (EquiClass e : getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB)) {
            sb.append(e.toString());
        }
        return sb.toString();
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
            color = "black";

            if (n.isAtomic())
                color = "green";

            if (n.isNested())
                color = "red";

            sb.append("\tn" + n.getId() + " [color=" + color + ",shape=\"" +
                    shape + "\"," + label + "=\"" + n.getDotLabel() + "\"];\n");
        }

        String option = "";
        String ecolor = "black";
        String par = "";

        //LOGGER.debug("eset {}", edgeSet().size());

        for (EquiEdge e : this.edgeSet()) {
            EquiClass src = e.getSource();
            EquiClass dest = e.getTarget();

            LOGGER.debug("src {}", src.getDotLabel());

            //assert outgoingEdgesOf(src).contains(e);
            //assert incomingEdgesOf(dest).contains(e);

            assert src != null;
            assert dest != null;

            ecolor = "black";
            par = "";

            if (e.getKind() == EquiEdge.Kind.SPLIT) {
                ecolor = "blue";
                par = "" + e.getSequence();
            } else if (e.getKind() == EquiEdge.Kind.SUB) {
                ecolor = "orange";
            } else if (e.getKind() == EquiEdge.Kind.INEQ) {
                ecolor = "red";
            }


            sb.append("\tn" + src.getId() + " -> n" + dest.getId() +
                    "[color=\"" + ecolor + "\",label=\"" + par.trim() + "\"];\n");

        }
        sb.append("}");
        return sb.toString();
    }

}

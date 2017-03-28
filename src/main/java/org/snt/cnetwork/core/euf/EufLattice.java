package org.snt.cnetwork.core.euf;


import org.apache.commons.collections.CollectionUtils;
import org.jgrapht.graph.DirectedPseudograph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

//@TODO: Julian simplify the implementation later on. Split the code in Model
// and Controller
public class EufLattice extends DirectedPseudograph<EquiClass, EquiEdge> implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(EufLattice.class);

    private EquiClass top = new EquiClass.Top();
    private EquiClass bottom = new EquiClass.Bottom();
    private EquiEdge init = new EquiEdge(top, bottom, EquiEdge.Kind.SUB, -1);
    private Map<String, EquiClass> lmap = new HashMap<>();
    private EufEventHandler eh = null;


    public EufLattice(EufEventHandler eh) {
        super(new EdgeFact());
        super.addVertex(top);
        super.addVertex(bottom);
        super.addEdge(top, bottom, init);
        lmap.put(bottom.toString(), bottom);
        lmap.put(top.toString(), top);
        this.eh = eh;
    }


    public EufLattice(EufEventHandler eh, EufLattice other) {
        this(eh);
        for(EquiEdge e : other.edgeSet()) {
            EquiClass esrc = e.getSource().clone();
            EquiClass edst = e.getTarget().clone();
            addEdge(esrc, edst, e.getKind(), e.getSequence());
        }
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

        if (isAlreadySubsumed(e))
            return;

        EquiClass mo = e;
        LOGGER.debug("insert {}:{}", e.getDotLabel(), e.getId());

        if(e.isNested()) {

            LOGGER.debug("INFERR {}", e.getDotLabel());
            Set<EquiClass> other = inferEquiClassFor(e);
            assert other.size() == 1;

            EquiClass o = other.iterator().next();

            LOGGER.debug("OTHER {}", o.getDotLabel());
            mo = mo.union(other.iterator().next());
        }


        // 1. geneerate max overlap
        try {
            mo = mo.union(getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB).
                    stream().filter(n -> n.hasOverlap(e)
            ).reduce(EquiClass::union).get());
        } catch (NoSuchElementException ex) {
        }

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


        // split the equi class in order to learn interesting facts
        // by analyzing nested equi classes


        if (sub.isEmpty()) {
            LOGGER.debug("SUB IS EMPTY");
            linkToTop(mo);
        } else {
            replace(sub, mo);
        }
        split(mo);
    }




    private EquiClass findSubsumptionPoint(EquiClass e) {

        EquiClass cursor = bottom;

        while (!cursor.subsumes(e)) {

            try {
                cursor = incomingEdgesOf(cursor).stream()
                        .map(EquiEdge::getSource).
                                filter(s -> s.hasOverlap(e)).findFirst().get();
            } catch (NoSuchElementException ex) {
                // if there is no overlap, we know that Node has to be the parent
                return top;
            }
        }
        return cursor;
    }


    public EquiClass getOverlapping(EquiClass o) {
        try {
            return getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB).stream()
                    .filter(x -> x.hasOverlap(o)).findFirst().get();
        } catch (NoSuchElementException e) {
            return o;
        }
    }

    /**
     * there can only be one covering equi class
     * @param o
     * @return
     */
    public EquiClass getCovering(EquiClass o) {

        LOGGER.debug("lmap {}", debug());

        // quicker access
        if(lmap.containsKey(o.getLabel()))
            return lmap.get(o.getLabel());

        try {
            return getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB).stream()
                    .filter(x -> x.subsumes(o)).findFirst().get();
        } catch (NoSuchElementException e) {
            return o;
        }
    }

    public boolean hasCovering(EquiClass o) {

        return getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB).stream()
                .filter(x -> x.subsumes(o)).count() > 0;
    }


    private void split(EquiClass n) throws EUFInconsistencyException {

        LinkedList<EquiClass> worklist = new LinkedList<>();
        LOGGER.debug("split {}:{}", n.getDotLabel(), n.getId());


        // n is a non-nested class
        if (!n.isNested()) {

            Set<EquiClass> nsub = n.split().stream().filter(e -> e.isNested())
                    .collect(Collectors.toSet());

            // no nested elements contained
            if (nsub.isEmpty())
                return;


            for (EquiClass s : nsub) {
                //Set<EquiEdge> e = getAllEdges(n, s);

                //if (e == null || e.isEmpty() || e.stream()
                //        .filter(x -> x.getKind() == EquiEdge.Kind.SUB).count
                //                () == 0) {
                //LOGGER.debug("parent {}:{} -> child {}:{}", n.getDotLabel
                //        (), n.getId(), s.getDotLabel(), s.getId());
                addSubEdge(n, s);
                worklist.add(s);
                //}
                //nsub.forEach(s -> addSubEdge(n, s));
            }

            //worklist.addAll(nsub);

        } else {
            worklist.add(n);
        }


        while (!worklist.isEmpty()) {
            EquiClass next = worklist.pop();
            worklist.addAll(handleNestedElement(next));
        }
        //LOGGER.debug(toDot());

    }


    private Set<EquiClass> handleNestedElement(EquiClass parent) throws EUFInconsistencyException {
        int x = 0;

        //LOGGER.debug(this.toDot());
        //LOGGER.debug("llmap {}", debug());
        //LOGGER.debug("handle nested element {}:{}", parent.getDotLabel(),
         //       parent.getId());
        Set<EquiClass> ret = new HashSet<>();

        assert parent.isNested();


        // this is all the parameters
        for (EquiClass n : parent.split()) {
            EquiClass alias = getOverlapping(n);

            //LOGGER.debug("Alias for {}:{} is {}:{}", n.getDotLabel(), n
            //                .getId(),
            //        alias.getDotLabel(), alias.getId());

            assert !parent.equals(alias);

            addSplitEdge(parent, alias, ++x);

            if (n.isNested()) {
                ret.add(n);
            }

        }
        assert x == parent.split().size();
        return ret;
    }


    protected EquiClass addEquiClass(Collection<EquiClass> toadd) throws
            EUFInconsistencyException {


        EquiClass eq = union(toadd);

        for (EquiClass e : toadd) {
            //LOGGER.debug(">> {}", e.getDotLabel());
            addEquiClass(e);
        }
        return findSubsumptionPoint(eq);
    }


    protected EquiClass addEquiClass(EquiClass n)
            throws EUFInconsistencyException {

        LOGGER.debug("add equi class {}:{}", n.getDotLabel(), n.getId());


        if (isAlreadySubsumed(n) || n.isEmpty()) {
            LOGGER.debug("{}:{} already subsumed", n.getLabel(), n.getId());
            return getCovering(n);
        }

        LOGGER.debug("find max ov for {}", n.getDotLabel());
        insert(n);

        if (edgeSet().contains(init)) {
            removeEdge(init);
            //LOGGER.debug("rm init edge {}", init);
        }

        if(eh != null) {
            eh.onEquiClassAddition(n);
        }

        return n;
    }


    protected Set<EquiEdge> outgoingEdgesOfKind(EquiClass e, EquiEdge.Kind
            kind) {
        Set<EquiEdge> ret = new HashSet<>();
        if (vertexSet().contains(e)) {
            ret.addAll(outgoingEdgesOf(e).stream().filter(ne -> ne.getKind() ==
                    kind).collect(Collectors.toSet()));
        }
        return ret;
    }

    protected Set<EquiEdge> incomingEdgesOfKind(EquiClass e, EquiEdge.Kind
            kind) {
        Set<EquiEdge> ret = new HashSet<>();
        if (vertexSet().contains(e)) {
            ret.addAll(incomingEdgesOf(e).stream().filter(ne -> ne.getKind() ==
                    kind)
                    .collect(Collectors.toSet()));
        }
        return ret;
    }

    protected Set<EquiClass> getConnectedOutNodesOfKind(EquiClass e, EquiEdge
            .Kind kind) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        ret.addAll(outgoingEdgesOfKind(e, kind).stream().map(EquiEdge::getTarget)
                .collect(Collectors.toSet()));
        return ret;
    }

    protected Set<EquiClass> getConnectedInNodesOfKind(EquiClass e, EquiEdge
            .Kind kind) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        ret.addAll(incomingEdgesOfKind(e, kind).stream().map(EquiEdge::getSource)
                .collect
                        (Collectors.toSet()));
        return ret;
    }


    protected Set<EquiClass> getConnectedOutNodes(EquiClass e) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        ret.addAll(outgoingEdgesOf(e).stream().map(EquiEdge::getTarget)
                .collect(Collectors.toSet()));
        return ret;
    }

    protected Set<EquiClass> getConnectedInNodesOf(EquiClass e) {
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
            updateCache(v);
            super.addVertex(v);
        }
        assert lmap.containsKey(v.getLabel());
        assert super.containsVertex(v);
        return lmap.get(v.getLabel());
    }

    private void addSplitEdge(EquiClass src, EquiClass dst, int idx) throws
            EUFInconsistencyException {
        //LOGGER.debug("add split edge {}:{} -> {}:{}",
        //        src.getDotLabel(), src.getId(),
        //        dst.getDotLabel(), dst.getId());

        assert src.isNested();

        addEdge(src, dst, EquiEdge.Kind.SPLIT, idx);

        if (inSubDegreeOf(src) == 0)
            linkToTop(src);

        if (inSubDegreeOf(dst) == 0)
            linkToTop(dst);

        if (outSubDegreeOf(dst) == 0)
            linkToBottom(dst);

    }


    private void addSubEdge(EquiClass src, EquiClass dst) {
        //LOGGER.debug("add sub edge {} -> {}", src.getDotLabel(), dst
        //        .getDotLabel());

        //assert  src.subsumes(dst);

        // no link to bottom require anymore
        removeEdge(src, bottom);

        addEdge(src, dst, EquiEdge.Kind.SUB, -1);


        if (inSubDegreeOf(src) == 0)
            linkToTop(src);

        if (outSubDegreeOf(dst) == 0)
            linkToBottom(dst);
    }

    protected void addIneqEdge(EquiClass src, EquiClass dst) {
        LOGGER.debug("add sub edge {} -> {}", src, dst);
        addEdge(src, dst, EquiEdge.Kind.INEQ, -1);
    }


    protected boolean hasIncomingEdgesOfKind(EquiClass n, EquiEdge.Kind kind) {
        Set<EquiEdge> in = incomingEdgesOfKind(n, kind);
        return in != null && in.size() > 0;
    }

    protected boolean hasOutgoingEdgesOfKind(EquiClass n, EquiEdge.Kind kind) {
        Set<EquiEdge> out = outgoingEdgesOfKind(n, kind);
        return out != null && out.size() > 0;
    }


    protected EquiClass join(EquiClass... e) {

        Objects.requireNonNull(e, "join cannot be called with a null " +
                "parameter");

        EquiClass u = union(Arrays.asList(e));

        return findSubsumptionPoint(u);
    }

    protected Collection<EquiClass> getCoveringSplit(EquiClass e) {
        return e.split().stream().map(v ->
                getOverlapping(v)).collect(Collectors.toList());
    }

    // given a nested equiclass infer the equivalent based on parameter
    // equivalence
    protected Set<EquiClass> inferEquiClassFor(EquiClass ec) throws EUFInconsistencyException {
        //assert e.isNested();
        //assert e.getElements().size() == 1;

        // first -- search for the corresponding equi class
        // if it does exist
        EquiClass e = getCovering(ec);

        if(!e.isNested())
            return Collections.singleton(e);

        Predicate<EquiEdge> p = k -> k.getKind() == EquiEdge.Kind.SPLIT;

        Set<EquiClass> ret = new HashSet<>();

        LOGGER.debug("infer equivalence class for {}", e.getDotLabel());

        final String anno = e.getElements().iterator().next().getAnnotation();


        // if there exist an alias, it has to be among this elems
        Set<EquiClass> srccrit = vertexSet().stream()
                .filter(v -> v.isNested())
                .filter(v -> !v.equals(e))
                .filter(v -> v.getElements().iterator().next().getAnnotation
                        ().equals(anno)).collect(Collectors.toSet());

        LOGGER.debug("srccrit {}", srccrit);

        // do not consider join elements from e as source crits
        if (containsVertex(e))
            srccrit.removeAll(bwslice(Collections.singleton(e), p));


        if (srccrit.isEmpty()) {
            ret.add(e);
            return ret;
        }

        LOGGER.debug("srccrit {}", srccrit);
        assert e.getElements().size() == 1;

        LOGGER.debug("card {}", e.getCardinality());

        LOGGER.debug("spl {}", e.split().size());


        // an ordered list of v's parameters
        Collection<EquiClass> plist = getCoveringSplit(e);

        assert plist.size() == e.split().size();

        LOGGER.debug("plist {}", plist.size());

        Set<EquiClass> tcrit = new HashSet<>();


        for (EquiClass par : plist) {

            LOGGER.debug("check par {}:{}", par.getDotLabel(), par.getId());

            if (!containsVertex(par)) {
                ret.add(e);
                return ret;
            }

            Set<EquiClass> check = incomingEdgesOf(par).stream()
                    .filter(t -> t.getKind() == EquiEdge.Kind.SPLIT)
                    .filter(t -> !t.getSource().equals(top))
                    .filter(t -> !t.getSource().equals(e))
                    .filter(t -> t.getSource().isNested())
                    .filter(t ->
                            CollectionUtils.isEqualCollection(getCoveringSplit(t.getSource()),
                                    plist))
                    .map(EquiEdge::getTarget)
                    .collect(Collectors.toSet());


            tcrit.addAll(check);
        }


        LOGGER.debug("tcrit {}", tcrit);

        if (srccrit.isEmpty() || tcrit.isEmpty()) {

            LOGGER.debug("tcrit, scrcrit empty");
            ret.add(e);
            return ret;
        }

        Set<EquiClass> fw = fwslice(srccrit, p);
        Set<EquiClass> bw = bwslice(tcrit, p);

        LOGGER.debug("FW {}", fw);
        LOGGER.debug("BW {}", bw);

        Set<EquiClass> chop = new HashSet<>();
        chop.addAll(fw);
        chop.retainAll(bw);


        try {
            chop = chop.stream()
                    .filter(v -> v.isNested())
                    .filter(v -> !v.equals(top))
                    .filter(v -> !v.equals(bottom))
                    .filter(v -> v.getElements().iterator().next().getAnnotation
                            ().equals(anno)).collect(Collectors.toSet());
        } catch (NoSuchElementException x) {
            ret.add(e);
            return ret;
        }

        LOGGER.debug("chop {}", chop);


        if (chop.isEmpty()) {
            ret.add(e);
            return ret;
        } else {

            //if(chop.size() > 1) {
            //   EquiClass nec = chop.stream().reduce(EquiClass::union).get()
            //           .union(e);
            //   addEquiClass(nec);
            //   return Collections.singleton(getCovering(nec));
            //} else {

                return chop;
            //}
        }
    }

    private Set<EquiClass> bwslice(Collection<EquiClass> crit,
                                   Predicate<EquiEdge> p) {
        LOGGER.debug("bw slice");
        LinkedList<EquiClass> wlist = new LinkedList<>();
        wlist.addAll(crit);
        Set<EquiClass> slice = new LinkedHashSet<>();
        while (!wlist.isEmpty()) {
            EquiClass ec = wlist.pop();

            if (slice.contains(ec))
                continue;

            slice.add(ec);
            if (inDegreeOf(ec) > 0) {

                wlist.addAll(incomingEdgesOf(ec).stream().filter(p).map(e -> e
                        .getSource()
                ).collect(Collectors.toSet()));

            }
        }
        return slice;
    }

    private Set<EquiClass> fwslice(Collection<EquiClass> crit,
                                   Predicate<EquiEdge> p) {
        LOGGER.debug("fw slice");
        LinkedList<EquiClass> wlist = new LinkedList<>();
        wlist.addAll(crit);
        Set<EquiClass> slice = new LinkedHashSet<>();

        while (!wlist.isEmpty()) {
            EquiClass ec = wlist.pop();

            if (slice.contains(ec))
                continue;

            slice.add(ec);
            if (outDegreeOf(ec) > 0) {
                wlist.addAll(outgoingEdgesOf(ec).stream().filter(p).map(e
                        -> e.getTarget()
                ).collect(Collectors.toSet()));
            }
        }

        return slice;
    }

    private boolean isAlreadySubsumed(EquiClass n) {

        return outgoingEdgesOfKind(top, EquiEdge.Kind.SUB).stream().map
                (EquiEdge::getTarget).filter(t -> t.subsumes(n)).count() > 0;
    }


    private Set<EquiEdge> replace(EquiClass toReplace, EquiClass replacement) {

        Set<EquiEdge> edges = new HashSet<>();


        Set<EquiEdge> out = outgoingEdgesOf(toReplace);
        Set<EquiEdge> in = incomingEdgesOf(toReplace);


        if (toReplace.isNested()) {
            // the replacement has to be split anyway so we do not consider
            // split edges here
            edges.addAll(in.stream()
                    //.filter(e -> e.getKind() == EquiEdge.Kind.SUB)

                    .map(e -> new EquiEdge(e.getSource(), replacement, e.getKind(), e.getSequence())
                    ).collect(Collectors.toSet()));

            //LOGGER.debug("OUT");
            edges.addAll(out.stream()
                    .filter(e -> e.getKind() == EquiEdge.Kind.SUB)
                    .map(e -> new EquiEdge(replacement, e.getTarget(), e.getKind(), e.getSequence())
                    ).collect(Collectors.toSet()));
        } else {
            // we consider all edges here
            edges.addAll(in.stream()
                    .map(e -> new EquiEdge(e.getSource(), replacement, e.getKind(), e.getSequence())
                    ).collect(Collectors.toSet()));

            //LOGGER.debug("OUT");
            edges.addAll(out.stream()
                    .map(e -> new EquiEdge(replacement, e.getTarget(), e.getKind(), e.getSequence())
                    ).collect(Collectors.toSet()));
        }

        //lmap.replace(toReplace.getLabel(), replacement);

        return edges;

    }

    private void replace(Set<EquiClass> toReplace, EquiClass replacement)
            throws EUFInconsistencyException {

        if (toReplace.isEmpty())
            return;


        LOGGER.debug("REPLACE");

        LOGGER.debug("+BEFORE &&&&&&&&&&&&&&&&&&&&&&&&&");
        LOGGER.debug(this.toDot());
        LOGGER.debug("-BEFORE &&&&&&&&&&&&&&&&&&&&&&&&&");



        for (EquiClass t : toReplace) {
            LOGGER.debug("to relink {}:{}", t.getDotLabel(), t.getId());
        }

        LOGGER.debug("replacement {}:{}", replacement.getDotLabel(),
                replacement.getId());

        Set<EquiEdge> edges = new HashSet<>();


        for (EquiClass torep : toReplace) {
            edges.addAll(replace(torep, replacement));
        }


        addEdges(edges);

        removeEquiClasses(toReplace);

        updateCache(edges);
    }

    private void updateCache(Set<EquiEdge> e) {
        e.forEach(x -> updateCache(x));
    }

    private void updateCache(EquiEdge e) {
        updateCache(e.getSource());
        updateCache(e.getTarget());
    }

    private void updateCache(EquiClass v) {
        lmap.put(v.getLabel(), v);

        if(!v.isNested() && !v.isSingleton()) {
            for (Element vc : v.getElements()) {
                lmap.put(vc.getLabel(), v);
            }
        }
    }

    private void removeEquiClasses(Collection<EquiClass> v) {
        v.forEach(e -> removeEquiClass(e));
    }

    private void removeEquiClass(EquiClass v) {
        //LOGGER.debug("remove vertex {}:{}", v.getDotLabel(), v.getId());
        lmap.remove(v.getLabel());
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
       //LOGGER.debug("add edge {}:{} -> {}:{}", src.getDotLabel(), src.getId
       //         (), dst.getDotLabel(), dst.getId());

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

            //LOGGER.debug("src {}", src.getDotLabel());

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


    public EquiClass getEquiClassByLabel(String l) {
        LOGGER.debug("1 {}", l);
        //LOGGER.debug("lmap ++ {}", debug());
        //LOGGER.debug(toDot());
        assert lmap.containsKey(l);
        LOGGER.debug("2");
        return lmap.get(l);
    }

    public boolean hasNodeForLabel(String l) {
        return lmap.containsKey(l);
    }

    public String debug() {
       //return lmap.toString();
        return "";
    }

}

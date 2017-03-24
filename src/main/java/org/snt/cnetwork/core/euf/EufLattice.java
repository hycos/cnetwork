package org.snt.cnetwork.core.euf;


import org.apache.commons.collections.CollectionUtils;
import org.jgrapht.graph.DirectedPseudograph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;
import org.snt.cnetwork.utils.BiMap;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

//@TODO: Julian simplify the implementation later on. Split the code in Model
// and Controller
public class EufLattice extends
        DirectedPseudograph<EquiClass, EquiEdge> implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(EufLattice.class);

    private EquiClass top = new EquiClass.Top();
    private EquiClass bottom = new EquiClass.Bottom();
    private EquiEdge init = new EquiEdge(top, bottom, EquiEdge.Kind.SUB, -1);
    private EquiClassFact elementFact = null;

    private BiMap<String, EquiClass> lmap = new BiMap<>();


    public EufLattice(EquiClassFact elementFact) {
        super(new EdgeFact());
        super.addVertex(top);
        super.addVertex(bottom);
        super.addEdge(top, bottom, init);
        this.elementFact = elementFact;
        lmap.put(bottom.toString(), bottom);
        lmap.put(top.toString(), top);
    }


    class IdComparator implements Comparator<Element> {
        @Override
        public int compare(Element o1, Element o2) {
            int id1 = o1.getMappedNode().getId();
            int id2 = o2.getMappedNode().getId();
            return id1 - id2;
        }
    }

    public EufLattice(EufLattice other, EquiClassFact elementFact) {
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
    public void addInequialityConstraint(Node... e) throws EUFInconsistencyException {

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

        EquiClass fst = getOverlapping(ec[0]);
        EquiClass snd = getOverlapping(ec[1]);


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

    private EquiClass addEquiClass(Node toadd)
            throws EUFInconsistencyException {
        return addEquiClass(elementFact.createEquiClass(toadd));
    }

    public EquiClass addEquiClass(Node... toadd)
            throws EUFInconsistencyException {
        assert elementFact != null;
        return addEquiClass(elementFact.createEquiClasses(toadd));
    }

    public EquiClass join(Node... e) throws MissingItemException {
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


    private void removeRendundancies(EquiClass c) throws
            EUFInconsistencyException {
        // @TODO:Julian important
        //addEquiClass(c);

        EquiClass covering = getOverlapping(c);


        // group by annotation
        Map<String, LinkedList<Element>> ng = new HashMap<>();

        LinkedList<Element> vars = new LinkedList<>();

        LinkedList<Element> con = new LinkedList<>();


        for (Element ta : covering.getElements()) {
            if (ta.isNested()) {
                if (!ng.containsKey(ta.getAnnotation())) {
                    ng.put(ta.getAnnotation(), new LinkedList<>());
                }
                ng.get(ta.getAnnotation()).add(ta);


                LOGGER.debug("add for {}", ta.getAnnotation());
                LOGGER.debug("SIZE {}", ng.get(ta.getAnnotation()));

            } else if (ta.isSingleton()) {

                if (ta.getMappedNode().isLiteral()) {

                    if (con.size() == 0)
                        con.add(ta);

                    if (con.size() == 1)
                        assert ta.getMappedNode().getId()
                                == con.getFirst().getMappedNode().getId();

                } else {
                    vars.add(ta);
                }
            }
            //LOGGER.debug("class:{}, ta element {}", c.getDotLabel(), ta
            //        .getLabel());
        }

        LOGGER.debug("ngroup to merge {}", ng.size());

        Set<LinkedList<Element>> nestedToMerge = ng.values().stream().filter(
                s -> s.size() > 1
        ).collect(Collectors.toSet());

        LOGGER.debug("nested to merge {}", nestedToMerge.size());


        // remove redundant nodes in the CN
        // 1. searching for equivalent functions of the same type (e.g.
        // indexof(a,b,c), indexof (d,e,f)
        // 2. remvoing the redundant node form the CN
        // 3. remapping the corresponding EUF label to the remaining node
        for (LinkedList<Element> ne : nestedToMerge) {
            Element min = Collections.min(ne, new IdComparator());
            ne.remove(min);
            remap(min, ne);
        }

        if (con.size() > 1) {
            throw new EUFInconsistencyException("there cannot be two literals" +
                    " belonging to the same equiclass " + con.toString());
        } else if (con.size() == 1 && vars.size() > 0) {
            // remap all variables to the constant value
            remap(con.iterator().next(), vars);
        } else if (vars.size() > 1) {
            Element min = Collections.min(vars, new IdComparator());
            vars.remove(min);
            remap(min, vars);
        }
    }


    // map all elements in list to the one at the first positon
    private void remap(Element firste,
                       Collection<Element> toremap) {

        Node first = firste.getMappedNode();


        LOGGER.debug("FRIST {}", firste.getLabel());

        assert !toremap.contains(first);

        for (Element e : toremap) {
            LOGGER.debug("REMAP {}", e.getLabel());

            Node mapped = e.getMappedNode();

            if (mapped.getId() != first.getId()) {
                elementFact.relink(mapped, first);
                e.setMappedNode(first);
            }

        }
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

        // quicker access
        if(lmap.containsKey(o.getLabel()))
            return lmap.getValueByKey(o.getLabel());

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
                LOGGER.debug("parent {}:{} -> child {}:{}", n.getDotLabel
                        (), n.getId(), s.getDotLabel(), s.getId());
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
        LOGGER.debug(toDot());

    }


    private Set<EquiClass> handleNestedElement(EquiClass parent) throws EUFInconsistencyException {
        int x = 0;

        LOGGER.debug(this.toDot());
        LOGGER.debug("handle nested element {}:{}", parent.getDotLabel(),
                parent.getId());
        Set<EquiClass> ret = new HashSet<>();

        assert parent.isNested();


        // this is all the parameters
        for (EquiClass n : parent.split()) {
            EquiClass alias = getOverlapping(n);

            LOGGER.debug("Alias for {}:{} is {}:{}", n.getDotLabel(), n.getId(),
                    alias.getDotLabel(), alias.getId());

            assert !parent.equals(alias);

            addSplitEdge(parent, alias, ++x);

            if (n.isNested()) {
                ret.add(n);
            }

        }
        assert x == parent.split().size();
        return ret;
    }


    public EquiClass addEquiClass(Collection<EquiClass> toadd) throws
            EUFInconsistencyException {


        EquiClass eq = union(toadd);

        for (EquiClass e : toadd) {
            //LOGGER.debug(">> {}", e.getDotLabel());
            addEquiClass(e);
        }
        return findSubsumptionPoint(eq);
    }


    public EquiClass addEquiClass(EquiClass n)
            throws EUFInconsistencyException {

        LOGGER.debug("add equi class {}:{}", n.getDotLabel(), n.getId());


        if (isAlreadySubsumed(n) || n.isEmpty()) {
            LOGGER.debug("{}:{} already subsumed", n.getLabel(), n.getId());
            return getOverlapping(n);
        }

        LOGGER.debug("find max ov for {}", n.getDotLabel());
        insert(n);

        if (edgeSet().contains(init)) {
            removeEdge(init);
            //LOGGER.debug("rm init edge {}", init);
        }

        removeRendundancies(n);



        return n;

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
        if (!lmap.containsKey(v.getLabel())) {
            lmap.put(v.getLabel(), v);
            super.addVertex(v);
        }
        assert lmap.containsKey(v.getLabel());
        assert super.containsVertex(v);
        return lmap.getValueByKey(v.getLabel());
    }

    private void addSplitEdge(EquiClass src, EquiClass dst, int idx) throws
            EUFInconsistencyException {
        LOGGER.debug("add split edge {}:{} -> {}:{}",
                src.getDotLabel(), src.getId(),
                dst.getDotLabel(), dst.getId());

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
        LOGGER.debug("add sub edge {} -> {}", src.getDotLabel(), dst.getDotLabel());

        //assert  src.subsumes(dst);

        // no link to bottom require anymore
        removeEdge(src, bottom);

        addEdge(src, dst, EquiEdge.Kind.SUB, -1);


        if (inSubDegreeOf(src) == 0)
            linkToTop(src);

        if (outSubDegreeOf(dst) == 0)
            linkToBottom(dst);
    }

    private void addIneqEdge(EquiClass src, EquiClass dst) {
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

    private Collection<EquiClass> getCoveringSplit(EquiClass e) {
        return e.split().stream().map(v ->
                getOverlapping(v)).collect(Collectors.toList());
    }

    // given a nested equiclass infer the equivalent based on parameter
    // equivalence
    public Set<EquiClass> inferEquiClassFor(EquiClass ec) {
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
            return chop;
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

            LOGGER.debug("OUT");
            edges.addAll(out.stream()
                    .filter(e -> e.getKind() == EquiEdge.Kind.SUB)
                    .map(e -> new EquiEdge(replacement, e.getTarget(), e.getKind(), e.getSequence())
                    ).collect(Collectors.toSet()));
        } else {
            // we consider all edges here
            edges.addAll(in.stream()
                    .map(e -> new EquiEdge(e.getSource(), replacement, e.getKind(), e.getSequence())
                    ).collect(Collectors.toSet()));

            LOGGER.debug("OUT");
            edges.addAll(out.stream()
                    .map(e -> new EquiEdge(replacement, e.getTarget(), e.getKind(), e.getSequence())
                    ).collect(Collectors.toSet()));
        }

        return edges;

    }

    private void replace(Set<EquiClass> toReplace, EquiClass replacement)
            throws EUFInconsistencyException {

        if (toReplace.isEmpty())
            return;


        LOGGER.debug("REPLACE");

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
    }


    private void removeEquiClasses(Collection<EquiClass> v) {
        v.forEach(e -> removeEquiClass(e));
    }

    private void removeEquiClass(EquiClass v) {
        LOGGER.debug("remove vertex {}:{}", v.getDotLabel(), v.getId());
        lmap.removeEntry(v.getLabel());
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
        LOGGER.debug("add edge {}:{} -> {}:{}", src.getDotLabel(), src.getId
                (), dst.getDotLabel(), dst.getId());

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

package org.snt.cnetwork.core.mergelattice;


import org.apache.commons.collections.map.HashedMap;
import org.jgrapht.graph.DirectedMultigraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;
import java.util.stream.Collectors;


public class MergeLattice<T> extends
        DirectedMultigraph<EquiClass, EquiEdge> implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(MergeLattice.class);

    private EquiClass top = new EquiClass.Top();
    private EquiClass bottom = new EquiClass.Bottom();
    private EquiEdge init = new EquiEdge(top, bottom, EquiEdge.Kind.SUB, -1);

    private EquiClassFact elementFact = null;

    private Map<EquiClass, EquiClass> equiLookup = new HashedMap();


    public void setEquiClassFact(EquiClassFact<T> elementFact) {
        this.elementFact = elementFact;
    }

    public MergeLattice(EquiClassFact<T> elementFact) {
        super(new EdgeFact());
        super.addVertex(top);
        super.addVertex(bottom);
        super.addEdge(top, bottom, init);
        this.elementFact = elementFact;
    }


    public MergeLattice(MergeLattice<T> other, EquiClassFact<T> elementFact) {
        this(elementFact);

        for(EquiClass e : other.vertexSet()) {
            try {
                addEquiClass(e);
            } catch (EUFInconsistencyException e1) {
                assert false;
            }
        }

        for(EquiEdge e : other.edgeSet()) {
            addEdge(e);
        }

    }

    /** API **/
    public void addInequialityConstraint(T ... e) throws EUFInconsistencyException {

        //LOGGER.debug("add inequality constraint");

        assert e.length == 2;


        if(!elementFact.hasEquiClassFor(e[0])) {
            LOGGER.debug("ADD 0");
            addEquiClass(e[0]);
        }

        if(!elementFact.hasEquiClassFor(e[1])) {
            LOGGER.debug("ADD 0");
            addEquiClass(e[1]);
        }
        EquiClass [] ec = new EquiClass[2];

        try {
            ec[0] = elementFact.getEquiClassFor(e[0]);
            ec[1] = elementFact.getEquiClassFor(e[1]);
        } catch (MissingItemException e1) {
            assert false;
        }

        assert ec.length == 2;

        EquiClass fst = findParent(ec[0]);
        EquiClass snd = findParent(ec[1]);

        if(fst.equals(snd))
            throw new EUFInconsistencyException("Inconsistency detected " +
                    "between " + fst + " " + snd);

        LOGGER.debug("fst par {}", fst);
        LOGGER.debug("snd par {}", snd);

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
        return addEquiClass(elementFact.createEquiClasses(toadd));
    }
    public EquiClass join(T ... e) throws MissingItemException {
        return join(elementFact.getEquiClassesFor(e));
    }
    public EquiClass meet(T ... e) throws MissingItemException {
        return meet(elementFact.getEquiClassesFor(e));
    }
    public Set<EquiClass> getPredecessorsOf(T n) {
        EquiClass e;
        try {
            e = elementFact.getEquiClassFor(n);
        } catch (MissingItemException e1) {
            LOGGER.error(e1.getMessage());
            return new HashSet<>();
        }
        return getPredecessorsOf(e);
    }
    public Set<EquiClass> getSuccessorsOf(T n) {
        EquiClass e = null;
        try {
            e = elementFact.getEquiClassFor(n);
        } catch (MissingItemException e1) {
            LOGGER.error(e1.getMessage());
            return new HashSet<>();
        }
        return getSuccesssorsOf(e);
    }
    public EquiClass getBottom() {
        return this.bottom;
    }
    public EquiClass getTop() {
        return this.top;
    }
    /** API **/

    private EquiClass findParent(EquiClass e) {

        EquiClass parent = top;

        //LOGGER.debug(":: Find parent for {}", e);

        if(vertexSet().contains(e)) {
            parent = join(e, bottom);
            LOGGER.debug(":: Preds {} bottom {}:: parent {}", getPredecessorsOf
                            (e), getPredecessorsOf(bottom),
                    parent);
        }

        //LOGGER.debug(":: parent is {}", parent);

        return parent;
    }


    private void insert(final EquiClass e) {

        // 1. geneerate max overlap
        EquiClass mo = e;
        try {
            mo = e.union(getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB).
                    stream().filter(n -> n.hasOverlap(e)
            ).reduce(EquiClass::union).get());
        } catch (NoSuchElementException ex) {
          ;
        }

        // 2. cleanup lattice
        EquiClass finalMo = mo;
        Set<EquiClass> sub = getConnectedOutNodesOfKind(top, EquiEdge.Kind
                .SUB)
                .stream()
                .filter(n -> finalMo.subsumes(n) && !finalMo.equals(n)).collect
                        (Collectors
                        .toSet());

        LOGGER.debug("MO {}", mo);

        // remove top edges
        //for(EquiClass s : sub) {
        //    addSubEdge(mo, s);
        //    removeEdge(top, s);
        //}

        replace(sub, mo);


        addSubEdge(top, mo);
        addSubEdge(mo,bottom);

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
                // if there is no overlap, we know that T has to be the parent
                return top;
            }
        }

        return cursor;
    }

    private boolean checkEUFConsistency(EquiClass e) {

        EquiClass cursor = top;

        Set<EquiClass> ret = new LinkedHashSet<>();

        for(EquiClass n : getConnectedOutNodesOfKind(cursor, EquiEdge.Kind
                .SUB)) {
            if(e.hasOverlap(n))
                ret.add(n);
        }

        return edgeSet().stream().filter(s -> s.getKind() == EquiEdge.Kind.INEQ)
                .filter(s -> ret.contains(s.getSource()) && ret.contains(s
                        .getTarget())).count() == 0;
    }


    private EquiClass getAlias(EquiClass o) {
        try {
            return getConnectedOutNodesOfKind(top, EquiEdge.Kind.SUB).stream()
                    .filter(x -> x.hasOverlap(o)).findFirst().get();
        } catch (NoSuchElementException e){
            return o;
        }
    }




    private void split(EquiClass n) {

        LinkedList<EquiClass> worklist = new LinkedList<>();

        worklist.add(n);

        while (!worklist.isEmpty()) {

            EquiClass parent = worklist.removeFirst();

            if (parent.isAtomic())
                continue;


            //LOGGER.debug("*** split {} {}", parent, parent.split().size());

            int idx = 0;

            for (EquiClass s : parent.split().stream().filter(
                    v -> v.isNested()
            ).collect(Collectors.toSet())) {


                Collection<EquiClass> pars = s.split();

                for(EquiClass p : pars) {
                    EquiClass alias = getAlias(p);
                    addSplitEdge(s, alias, ++idx);
                    addSubEdge(parent, s);
                }

                //if(parent.equals(alias))
                //    continue;

                //if (parent.isNested()) {
                //    EquiClass alias = getAlias(s);
                //    addSplitEdge(parent, alias, ++idx);
                //} else {
                    //LOGGER.debug("parent {} no single", parent);
                //    addSubEdge(parent, s);
                //    if (!s.isAtomic()) {
                //        worklist.addLast(s);
                //    }
                //}


            }
        }
    }


    public EquiClass addEquiClass(Collection<EquiClass> toadd) throws
            EUFInconsistencyException {

        for(EquiClass ec : toadd) {
            addEquiClass(ec);
        }

        return findSubsumptionPoint(toadd.iterator().next());
    }


    public void addEquiClass(EquiClass n)
            throws EUFInconsistencyException {



        LOGGER.debug("(+) add equi class {}", n);


        LOGGER.debug(toDot());

        if (equiLookup.containsKey(n) || n.isEmpty()) {
            LOGGER.debug("already there");
            return;
        }

        if (isAlreadySubsumed(n)) {
            LOGGER.debug("already subsumed");
            return;
        }

        if(!checkEUFConsistency(n)) {
            throw new EUFInconsistencyException(n + " constradicts with " +
                    "imposed inequality constraints");
        }


        LOGGER.debug("find max ov {}", n);
        insert(n);

        if (edgeSet().contains(init)) {
            removeEdge(init);
            //LOGGER.debug("rm init edge {}", init);
        }

    }


    public Set<EquiEdge> outgoingEdgesOfKind(EquiClass e, EquiEdge.Kind
            kind) {
        Set<EquiEdge> ret = new HashSet<>();
        if(vertexSet().contains(e)) {
            ret.addAll(outgoingEdgesOf(e).stream().filter(ne -> ne.getKind() ==
                    kind)
                    .collect(Collectors.toSet()));
        }
        return ret;
    }

    public Set<EquiEdge> incomingEdgesOfKind(EquiClass e, EquiEdge.Kind
            kind) {
        Set<EquiEdge> ret = new HashSet<>();
        if(vertexSet().contains(e)) {
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
        if (!equiLookup.containsKey(v)) {
            equiLookup.put(v, v);
            super.addVertex(v);
        }
        return equiLookup.get(v);
    }

    public void addSplitEdge(EquiClass src, EquiClass dst, int idx) {
        LOGGER.debug("add par edge {} -> {}", src, dst);
        addEdge(src, dst, EquiEdge.Kind.SPLIT, idx);

        if(inSubDegreeOf(dst) == 0)
            linkToTop(dst);

        if(outSubDegreeOf(dst) == 0)
            linkToBottom(dst);
    }


    public void addSubEdge(EquiClass src, EquiClass dst) {
        LOGGER.debug("add sub edge {} -> {}", src, dst);

        addEdge(src, dst, EquiEdge.Kind.SUB, -1);

        if(inSubDegreeOf(src) == 0)
            linkToTop(src);

        if(outSubDegreeOf(dst) == 0)
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

    private EquiClass getSingleSuperclass(EquiClass child) {
        assert incomingEdgesOfKind(child, EquiEdge.Kind.SUB).size() == 1;

        return incomingEdgesOfKind(child, EquiEdge.Kind.SUB).iterator().next
                ().getSource();
    }



    private EquiClass getAliases(EquiClass parent, EquiClass child) {

        LOGGER.debug("get aliases");
        EquiClass joinpt = join(parent, child);

        LOGGER.debug("JOIN of {} and {} is {}", parent, child, joinpt);

        // there is not join point (child is not a subset of parent)
        if (joinpt.equals(top))
            return getSingleSuperclass(child);

        //LOGGER.debug("SUB {}", incomingEdgesOfKind(child, EquiEdge.Kind.SUB));
        // prevent recursion
        Set<EquiClass> eq = incomingEdgesOfKind(child, EquiEdge.Kind.SUB)
                .stream().map
                        (EquiEdge::getSource).filter(s -> !s.equals(joinpt)).collect
                        (Collectors.toSet
                                ());
        //LOGGER.debug("EQQ {}", eq.toString());

        // every upwards connection goes through joinpt
        if (eq.isEmpty())
            return top;

        //LOGGER.debug("eq {}", eq.toString());
        assert eq.size() == 1;
        return eq.iterator().next();
    }


    private EquiClass nextSubMergePt() {
        try {
            return vertexSet().stream()
                    .filter(v ->
                            incomingEdgesOfKind(v, EquiEdge.Kind.SUB).size() > 1
                                    && !v.equals(bottom)
                    ).findFirst()
                    .get();
        } catch (NoSuchElementException | NullPointerException e) {
            return null;
        }
    }


    private Set<EquiClass> getTuplesToMerge() {
        // get functions first
        return vertexSet().stream().filter(v ->
                isTuple(v)).collect(Collectors.toSet());
    }

    private boolean isTuple(EquiClass e) {
        return hasOutgoingEdgesOfKind(e, EquiEdge.Kind.SPLIT);
    }

    private boolean isParam(EquiClass e) {
        return hasIncomingEdgesOfKind(e, EquiEdge.Kind.SPLIT);
    }

    private boolean isDirectSuccessorOfTop(EquiClass e) {
        return incomingEdgesOfKind(e, EquiEdge.Kind.SUB).stream().filter(
                k -> k.getSource().equals(top)
        ).count() > 0;
    }

    private Set<EquiClass> getPredecessorsOf(EquiClass e) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        LinkedList<EquiClass> q = new LinkedList<>();

        q.addAll(getConnectedInNodesOfKind(e, EquiEdge.Kind.SUB));

        while(!q.isEmpty()) {

            EquiClass cur = q.poll();

            if(!cur.equals(top))
                ret.add(cur);

            q.addAll(getConnectedInNodesOfKind(cur, EquiEdge.Kind.SUB));
        }

        ret.add(top);
        return ret;
    }

    private Set<EquiClass> getSuccesssorsOf(EquiClass e) {
        Set<EquiClass> ret = new LinkedHashSet<>();
        LinkedList<EquiClass> q = new LinkedList<>();

        q.addAll(getConnectedOutNodesOfKind(e, EquiEdge.Kind.SUB));

        while(!q.isEmpty()) {

            EquiClass cur = q.poll();

            if(!cur.equals(bottom))
                ret.add(cur);

            q.addAll(getConnectedOutNodesOfKind(cur, EquiEdge.Kind.SUB));
        }

        ret.add(bottom);
        return ret;
    }


    private EquiClass join(EquiClass ... e) {

        Set<EquiClass> ret = null;

        Objects.requireNonNull(e , "join cannot be called with a null " +
                "parameter");

        assert e.length > 0;

        for(EquiClass c : e) {

            assert c != null;

            if(ret == null) {
                ret = getPredecessorsOf(c);
            }

            ret.retainAll(getPredecessorsOf(c));
        }

        LOGGER.debug("joint result {}", ret);

        if(ret.isEmpty())
            return top;
        else
            return ret.iterator().next();
    }

    private EquiClass meet(EquiClass ... e) {

        Set<EquiClass> ret = null;

        for(EquiClass c : e) {
            if(ret == null) {
                ret = getSuccesssorsOf(c);
            }

            ret.retainAll(getSuccesssorsOf(c));
        }

        if(ret.isEmpty())
            return bottom;
        else
            return ret.iterator().next();
    }

    private void mergeSub() {
        LOGGER.debug("merge subclasses");
        EquiClass nxt;

        while ((nxt = nextSubMergePt()) != null) {

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


    private boolean isAlreadySubsumed(EquiClass n) {

        return outgoingEdgesOfKind(top, EquiEdge.Kind.SUB).stream().map
                (EquiEdge::getTarget).filter(t -> t.subsumes(n)).count() > 0;
    }


    private void replace(Set<EquiClass> toReplace, EquiClass replacement) {

        Set<EquiEdge> edges = toReplace.stream().map(v -> outgoingEdgesOf(v))
                .flatMap(x -> x.stream())
                .filter(e -> !replacement.equals(e.getTarget()))
                .map(e -> new EquiEdge(replacement, e
                        .getTarget(), e.getKind(), e
                        .getSequence())).collect(Collectors.toSet());

        edges.addAll(toReplace.stream().map(v -> incomingEdgesOf(v))
                .flatMap(x -> x.stream())
                .filter(e -> !replacement.equals(e.getSource()))
                .map(e -> new EquiEdge(e.getSource
                        (), replacement, e.getKind(), e
                        .getSequence())).collect(Collectors.toSet()));

        LOGGER.debug("toreplace {}", toReplace);
        LOGGER.debug("rpl {}", replacement);

        removeEquiClasses(toReplace);

        addEdges(edges);
        //split(replacement);
    }


    private void removeEquiClasses(Collection<EquiClass> v) {
        v.forEach(e -> removeEquiClass(e));
    }

    private void removeEquiClass(EquiClass v) {
        LOGGER.debug("remove vertex {}", v);
        super.removeVertex(v);

        //assert edgeSet().stream().filter(e -> e.getSource().equals(v) || e
        //        .getTarget().equals(v)).count() == 0;
        equiLookup.remove(v);
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

            if(n.isNested())
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
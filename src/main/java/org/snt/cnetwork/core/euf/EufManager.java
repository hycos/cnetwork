package org.snt.cnetwork.core.euf;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.ConstraintNetworkObserver;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.core.consistency.ConsistencyCheckerFactory;
import org.snt.cnetwork.core.domain.range.BooleanRange;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.exception.MissingItemException;

import java.util.*;
import java.util.stream.Collectors;

public class EufManager implements EufEventHandler,ConstraintNetworkObserver<Node> {


    final static Logger LOGGER = LoggerFactory.getLogger(EufManager.class);

    private EufLattice lattice = null;
    private NodeElemFact elementFact = null;
    private ConstraintNetworkBuilder cb = null;


    class IdComparator implements Comparator<Element> {
        @Override
        public int compare(Element o1, Element o2) {
            int id1 = o1.getMappedNode().getId();
            int id2 = o2.getMappedNode().getId();
            return id1 - id2;
        }
    }

    public EufManager(ConstraintNetworkBuilder cb) {
        this.lattice = new EufLattice(this);
        this.elementFact = new NodeElemFact(cb);
        this.cb = cb;
    }

    // copy constructor
    public EufManager(EufManager mgr, ConstraintNetworkBuilder cb) {
        // copy element fact and updating reference to new cn
        assert (mgr.lattice != null);
        this.lattice = new EufLattice(this, mgr.lattice);
        assert this.lattice != null;
        this.elementFact = new NodeElemFact(mgr.elementFact, cb);
        assert this.lattice.vertexSet().size() == mgr.lattice.vertexSet().size();
        this.cb = cb;
    }

    private boolean isRedundantPar(Node p, boolean val) {
        if (!p.isBoolean())
            return false;

        BooleanRange br = (BooleanRange) p.getRange();
        return (br.isAlwaysTrue() && val) || (br.isAlwaysFalse() && !val);
    }


    private Node[] getParList(Collection<Node> n, boolean val) {
        List<Node> r = n.stream().filter(x -> !isRedundantPar(x, val)).collect
                (Collectors
                        .toList());

        for (Node rr : r) {
            //LOGGER.debug("RR {}", rr.getLabel());
            //LOGGER.debug("EQ {}", getEquiClassForNode(rr)
            //        .getCorrespondingElement(rr).getMappedNode().getLabel());
        }
        return r.toArray(new Node[r.size()]);
    }

    private boolean hasRedundantPars(Collection<Node> ps, boolean val) {
        return ps.stream().filter(x -> isRedundantPar(x, val)).count() > 0;
    }

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


        ec[0] = elementFact.getEquiClassFor(e[0]);
        ec[1] = elementFact.getEquiClassFor(e[1]);

        assert ec.length == 2;

        EquiClass fst = lattice.getTopCovering(ec[0]);
        EquiClass snd = lattice.getTopCovering(ec[1]);


        //LOGGER.debug("fst {} par {}", ec[0].getDotLabel(), fst.getDotLabel());
        //LOGGER.debug("snd {} par {}", ec[1].getDotLabel(), snd.getDotLabel());


        // cannot create an ineq edge where source and dest are pointing to the
        // same equi class
        if (fst.equals(snd)) {
            throw new EUFInconsistencyException("Inconsistency detected " +
                    "between " + fst + " " + snd);
        }


        fst = fst.equals(lattice.getTop()) ? ec[0] : fst;
        snd = snd.equals(lattice.getTop()) ? ec[1] : snd;


        lattice.addIneqEdge(fst, snd);
        lattice.addIneqEdge(snd, fst);
    }

    private void removeOperandRedundancies(EquiClass c) throws EUFInconsistencyException {

        //LOGGER.debug("remove operand redundancies");
        assert lattice != null;

        EquiClass covering = lattice.getTopCovering(c);

        //LOGGER.debug("got covering {}", covering.getElements().size());
        //LOGGER.debug("covering {}", covering);

        // group by annotation
        Map<String, LinkedList<Element>> ng = new HashMap<>();

        LinkedList<Element> vars = new LinkedList<>();

        LinkedList<Element> con = new LinkedList<>();


        for (Element ta : covering.getElements()) {
            if (ta.isNested()) {

            } else if (ta.isSingleton()) {

                if (ta.getMappedNode().isLiteral()) {

                    if (con.size() == 0)
                        con.add(ta);

                    if (con.size() >= 1)
                        if (ta.getMappedNode().getId()
                                != con.getFirst().getMappedNode().getId()) {

                        //LOGGER.debug(lattice.toDot());
                            throw new EUFInconsistencyException(
                                    ta.getMappedNode().getLabel() +
                                            " should not be in the same " +
                                            "equiclass as " + con.getFirst()
                                            .getMappedNode().getLabel()
                            );
                        }

                } else {
                    vars.add(ta);
                }
            }
            //LOGGER.debug("class:{}, ta element {}", c.getDotLabel(), ta
            //        .getLabel());
        }

        LOGGER.debug("ngroup to merge {}", ng.size());

        if (con.size() > 1) {
            throw new EUFInconsistencyException("there cannot be two literals" +
                    " belonging to the same equiclass " + con.toString());
        } else if (con.size() == 1 && vars.size() > 0) {
            // remap all variables to the constant value
            remap(con.iterator().next(), vars);
        } else if (vars.size() > 1) {
            // Element min = Collections.min(vars, new IdComparator());
            Element min = findFirst(vars);
            if(min != null) {
                vars.remove(min);
                remap(min, vars);
            }
        }

    }


    private void removeOperationRedundancies(EquiClass c) throws
            EUFInconsistencyException {

        LOGGER.debug("remove operation redundancies");
        assert lattice != null;


        EquiClass covering = lattice.getTopCovering(c);

        LOGGER.debug("got covering {}", covering.getElements().size());

        // group by annotation
        Map<String, Map<Integer, LinkedList<Element>>> ng = new HashMap<>();

        for (Element ta : covering.getElements()) {
            int id = ta.split().length;

            if (ta.isNested()) {
                if (!ng.containsKey(ta.getAnnotation())) {
                    ng.put(ta.getAnnotation(), new HashMap<>());
                }

                if(!ng.get(ta.getAnnotation()).containsKey(id)) {
                    ng.get(ta.getAnnotation()).put(id, new
                            LinkedList<>());
                }

                ng.get(ta.getAnnotation()).get(id).add(ta);

                LOGGER.debug("add for {}", ta.getAnnotation());
                LOGGER.debug("SIZE {}", ng.get(ta.getAnnotation()));
            }
        }


        for (Map<Integer, LinkedList<Element>> e : ng.values()) {
            //Element min = Collections.min(ne, new IdComparator());
            // do not consider min object when remapping

            for(LinkedList<Element> ne : e.values()) {
                if(ne.size() > 1) {
                    LOGGER.debug("ngroup to merge {}", ne.size());
                    Element min = findFirst(ne);

                    if(min != null) {
                        ne.remove(min);

                        LOGGER.debug("remap {}:{}", min, ne);
                        remap(min, ne);
                    }
                }
            }
        }

    }

    // get the element wit the smallest id that is present in
    // the constraint network
    private Element findFirst(Collection<Element> ele) {
        try {
            return ele.stream().sorted(new IdComparator()).filter(e -> cb
                    .containsVertex(e.getMappedNode())).findFirst().get();
        } catch (NoSuchElementException e) {
            return null;
        }
    }


    // map all elements in list to the one at the first positon
    private void remap(Element firste, Collection<Element> toremap) throws
            EUFInconsistencyException {

        //LOGGER.debug(cb.getConstraintNetwork().toDot());

        Node first = firste.getMappedNode();

        //LOGGER.debug("FRIST {}:{}", firste.getLabel(), first.getId());

        assert cb.vertexSet().contains(first);

        assert !toremap.contains(first);

        for (Element e : toremap) {

            if (!cb.vertexSet().contains(e.getMappedNode())) {
                e.setMappedNode(first);
                continue;
            }

            assert cb.vertexSet().contains(e.getMappedNode());
//            LOGGER.debug("REMAP {}:{}:{}", e.getLabel(), e.getMappedNode()
//                    .getId(), e.getMappedNode().getRange());

            Node mapped = e.getMappedNode();

            if (mapped.getId() != first.getId()) {
                cb.merge(mapped, first);
                e.setMappedNode(first);
            }

        }

    }

    public EquiClass addEquiClass(Node... toadd)
            throws EUFInconsistencyException {
        assert elementFact != null;
        Collection<EquiClass> neqi = elementFact.createEquiClasses(toadd);
        EquiClass ec = lattice.addEquiClass(neqi);
        return ec;
    }

    @Override
    public void onEquiClassAddition(EquiClass ec) throws EUFInconsistencyException {
        removeOperandRedundancies(ec);
        //linkEqualNodes(ec);
    }

    @Override
    public void onEquiClassReplace(Set<EquiClass> old, EquiClass enew) throws
            EUFInconsistencyException {

//        Set<EquiClass> in = lattice.getConnectedInNodesOfKind(enew, EquiEdge
//                .Kind
//                .SPLIT);
//
//        LOGGER.debug("on equiclass add {}", enew);
//
//        for(EquiClass c : in) {
//            assert c.isNested();
//            Set<EquiClass> snen = lattice.inferEquiClassFor(c);
//            EquiClass nec = snen.stream().reduce(EquiClass::union).get()
//                    .union(c);
//
//            LOGGER.debug("nec {}", nec);
//
//            EquiClass nnec = addEquiClass(nec);
//
//            // remove redunancies just for the operations with parameter
//            // equivalence
//            //removeOperationRedundancies(nec);
//        }
    }


    public EquiClass join(Node... e) throws MissingItemException {
        return lattice.join(elementFact.getEquiClassesFor(e));
    }

    /**
     * infer the actual equiclass to which a node belongs to
     *
     * @param n
     * @return
     */
    public EquiClass inferActualEquiClassForNode(Node n) throws EUFInconsistencyException {

        //LOGGER.debug(lattice.toDot());
        elementFact.createEquiClass(n);
        EquiClass ec = elementFact.getEquiClassFor(n);
        Set<EquiClass> snen = lattice.inferEquiClassFor(ec);

        //LOGGER.debug("ieq {}:|{}| -- {}:{}", snen, snen.size(), ec, ec, n
        //        .getId());

        if (snen.size() >= 1) {
            EquiClass nec = snen.stream().reduce(EquiClass::union).get().union(ec);
            EquiClass nnec = addEquiClass(nec);

            // remove redunancies just for the operations with parameter
            // equivalence
            removeOperationRedundancies(nec);
            return nnec;
        }


        assert snen.size() == 1;
        return snen.iterator().next();
    }

    public EufLattice getLattice() {
        return this.lattice;
    }

    public EquiClass getTop() {
        return this.lattice.getTop();
    }

    public EquiClass getBottom() {
        return this.lattice.getBottom();
    }

    public EquiClass getEquiClassForNode(Node n) {
        return elementFact.getEquiClassFor(n);
    }

    public Node getNodeByLabel(String lbl) {
        //LOGGER.debug(this.getLattice().toDot());
        EquiClass ec = lattice.getEquiClassByLabel(lbl);
        LOGGER.debug(ec.getDotLabel());
        return ec.getFirstElement().getMappedNode();
    }

    public boolean hasNodeForLabel(String lbl) {
        return lattice.hasNodeForLabel(lbl);
    }

    public EquiClass addEquiClass(EquiClass e) throws EUFInconsistencyException {
        EquiClass c = lattice.addEquiClass(e);
        removeOperandRedundancies(c);
        return c;
    }

    public void update(Node n) throws EUFInconsistencyException {
        //LOGGER.debug(">> update {}", n.getDotLabel());
        //LOGGER.debug(">> update {}", n.getDotLabel());

        if(!cb.vertexSet().contains(n))
            return;

        //LOGGER.debug(this.cb.getConstraintNetwork().toDot());

        if (n.isNumeric() && n.getRange().isSingleton()) {


            /**LOGGER.debug("RAN " + n.getLabel() + " " + n.getRange()
                    .toString
                            ());
            elementFact.createEquiClass(n);

            EquiClass eq = null;

            eq = elementFact.getEquiClassFor(n).clone();


            eq.addElement(new SingletonElement(n, n.getRange().getMin()
                    .getEndpoint().toString()));

            LOGGER.debug("new eq {}", eq.toString());

            lattice.addEquiClass(eq);**/

        } else if (n.isString() && n.getAutomaton().isSingleton()) {

            /**elementFact.createEquiClass(n);

            EquiClass eq = null;

            eq = elementFact.getEquiClassFor(n).clone();


            eq.addElement(new SingletonElement(n, "\"" + n.getAutomaton
                    ().getShortestExample() + "\""));

            LOGGER.debug("new eq {}", eq.toString());

            lattice.addEquiClass(eq);**/
        } else if (n.isBoolean()) {

            if (n.getKind().isInequality()) {
                if (((BooleanRange) n.getRange()).isAlwaysFalse()) {
                    List<Node> pars = cb.getParametersFor(n);
                    assert pars.size() == 2;
                    addEquiClass(getParList(pars, false));
                }
                if (((BooleanRange) n.getRange()).isAlwaysTrue()) {
                    List<Node> pars = cb.getParametersFor(n);
                    assert pars.size() == 2;
                    if (!hasRedundantPars(pars, true))
                        addInequialityConstraint(pars.get(0), pars.get(1));
                }

            } else if (n.getKind().isEquality()) {
                LOGGER.debug("n {}", n.getDotLabel());
                assert n.getRange() instanceof BooleanRange;
                if (((BooleanRange) n.getRange()).isAlwaysTrue()) {

                    List<Node> pars = cb.inferParams(cb.getParametersFor(n));
                    assert pars.size() == 2;
                    addEquiClass(getParList(pars, true));
                }
                if (((BooleanRange) n.getRange()).isAlwaysFalse()) {
                    List<Node> pars = cb.getParametersFor(n);
                    assert pars.size() == 2;
                    if (!hasRedundantPars(pars, false))
                        addInequialityConstraint(pars.get(0), pars.get(1));
                }
            }
        }

        ConsistencyCheckerFactory.INSTANCE.checkConsistency(cb);

        //if (n.isOperation()) {
        //    addEquiClass(n);
        //}

    }


    public void attach(Node n) {
        LOGGER.debug("Attach listener to {}", n.getLabel());
        n.attach(this);
    }

}

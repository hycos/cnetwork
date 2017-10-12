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

package com.github.hycos.cnetwork.core.graph;


import com.github.hycos.cnetwork.core.domain.range.BooleanRange;
import com.github.hycos.cnetwork.core.euf.Element;
import com.github.hycos.cnetwork.core.euf.EquiClass;
import com.github.hycos.cnetwork.core.euf.EufManager;
import com.github.hycos.cnetwork.core.execdag.ExecDag;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.consistency.ConsistencyCheckerFactory;
import com.github.hycos.cnetwork.core.domain.NodeDomain;
import com.github.hycos.cnetwork.core.domain.NodeDomainFactory;
import com.github.hycos.cnetwork.core.euf.EufLattice;

import java.util.*;
import java.util.function.Predicate;


public class ConstraintNetworkBuilder implements Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkBuilder.class);

    private ConstraintNetwork cn;
    //private NodeElemFact nf;
    private EufManager euf;
    private ExecDag tree;

    // observer structural changes to on the cn
    private Set<ConstraintNetworkEventListener> listeners = new HashSet<>();

    // Observe variables and operations
    private Set<ConstraintNetworkObserver<Node>> observers = new HashSet<>();

    private void addDefaultContraint() {
        Node ntrue = new Operand("true", NodeKind.BOOLLIT);
        Node nfalse = new Operand("false", NodeKind.BOOLLIT);
        try {
            addConstraint(NodeKind.NEQUALS, ntrue, nfalse);
        } catch (EUFInconsistencyException e) {
            assert false;
        }
    }

    // Note that listeners are no copied
    public ConstraintNetworkBuilder(ConstraintNetworkBuilder cnb) {
        this.cn = new ConstraintNetwork(cnb.cn);
        //this.nf = new NodeElemFact(this, cnb.nf);
        this.euf = new EufManager(cnb.euf, this);
        this.tree = new ExecDag(cnb.tree, this);

        assert cnb.vertexSet().size() == this.cn.vertexSet().size();

        this.observers.add(this.euf);
        this.observers.add(this.tree);
        this.listeners.add(this.tree);

        attachObservers();
        //addDefaultContraint();
    }

    public ConstraintNetworkBuilder() {
        this.cn = new ConstraintNetwork();
        this.euf = new EufManager(this);
        this.tree = new ExecDag(this);
        this.observers.add(this.euf);
        this.observers.add(this.tree);
        this.listeners.add(this.tree);
        //addDefaultContraint();
    }

    private void attachObservers() {
        cn.vertexSet().forEach(v -> attachObservers(v));
    }

    private void attachObservers(Node n) {
        this.observers.forEach(o -> n.attach(o));
    }

    private void updateObservers(Node n) throws EUFInconsistencyException {

        LOGGER.debug("UPATE SIZ: " + n.getDomain().size());
        //assert n.getDomain().size() == 2;

        for(ConstraintNetworkObserver<Node> o : observers) {
            o.update(n);
        }
    }

    public Node getNodeById(int id) {
        return this.cn.getNodeById(id);
    }

    public Node getNodeByLabel(String lbl) {
        LOGGER.debug("get node by label {}", lbl);
        return this.euf.getNodeByLabel(lbl);
    }

    public boolean hasNodeForLabel(String lbl) {
        return this.euf.hasNodeForLabel(lbl);
    }

    public String getLabelForNode(Node n) {
        EquiClass e = euf.getEquiClassForNode(n);
        return e.getCorrespondingElement(n).getLabel();
    }

    public Node addConstraint(NodeKind kind, Node... params) throws
            EUFInconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addConstraint(kind, lst);
    }

    public Node addOperation(NodeKind kind, Node... params) throws
            EUFInconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addOperation(kind, lst);
    }

    public Edge addConnection(Edge e) throws EUFInconsistencyException {
        Edge ne = cn.addConnection(e);
        listeners.forEach(l -> l.onAddConnection(ne));
        return ne;
    }

    public void addListener(ConstraintNetworkEventListener listener) {
        this.listeners.add(listener);
    }

    public void addListener(Set<ConstraintNetworkEventListener> listeners) {
        this.listeners.addAll(listeners);
    }

    public Node addOperand(NodeKind n, String s) {

        //LOGGER.debug("add operand {}:{}", n, s);
        Node op = cn.addOperand(n, s);

        //LOGGER.debug("lbl for node {}:{}", op.getId(), op.getLabel());

        try {
            euf.addEquiClass(op);
            return infer(op);
        } catch (EUFInconsistencyException e) {
            LOGGER.error(e.getMessage());
            e.printStackTrace();
            assert false;
        }

        return null;
    }

    private Node infer(Node n) throws EUFInconsistencyException {

        Node nop = inferEquivalentNode(n);

        //LOGGER.debug("NOP {}:{}", nop, nop.getId());
        //LOGGER.debug("OP {}:{}", n, n.getId());

        assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);


        if (!ConsistencyCheckerFactory.INSTANCE.isConsistent(this, nop)) {
            throw new EUFInconsistencyException("malformed operand " + nop
                    .getKind());
        }

        if (n.equals(nop)) {
            attachObservers(nop);
            updateObservers(nop);
//            euf.attach(nop);
//            euf.update(nop);
            //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

            return nop;
        } else {

            //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);
            LOGGER.debug("BOOOOO {}", n.getId());
            //cn.removeVertex(n);

            //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

            return nop;
        }
    }

    public List<Node> inferParams(List<Node> params) throws
            EUFInconsistencyException {
        List<Node> plist = new Vector<>();
        LOGGER.debug("infer params");
        for(Node p : params) {

            Node i = infer(p);

            LOGGER.debug("inferred {}:{}", p.getId(), i.getId());
            plist.add(i);
        }

        assert plist.size() == params.size();
        return plist;
    }


    public Node addOperation(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {


        Node op = cn.addOperation(kind, inferParams(params));

        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);
        LOGGER.debug("check node {}:{}", op.getLabel(), op.getId());

        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

        //if(op.getId() == 233)
        //    System.exit(-1);
        Node nop = infer(op);

        LOGGER.debug("nop {}", nop.getId());
        LOGGER.debug("op {}", op.getId());

        // there is a node with a differnt
        if (nop.getId() != op.getId()) {
            LOGGER.debug("** remove {}", nop.getId());
            cn.removeVertex(op);
        }

        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

        return nop;
    }

    public Node addConstraint(NodeKind kind, List<Node> params) throws
            EUFInconsistencyException {

        Node op = addOperation(kind, params);

        BooleanRange br = (BooleanRange) op.getRange();

        if (br.isAlwaysFalse()) {
            throw new EUFInconsistencyException("UNSAT");
        }

        op.setDomain(NodeDomainFactory.DBTRUE);

        return op;
    }

    public Operation addExtOperation(String identifier, List<Node> params) {
        return cn.addExtOperation(identifier, params);
    }

    public boolean removeAllVertices(Collection<? extends Node> n) {

        for(Node nd : n) {
            removeVertex(nd);
        }

        return true;
    }

    public boolean removeAllEdges(Collection<? extends Edge> e) {
        return cn.removeAllEdges(e);
    }

    public Set<Edge> getAllEdges(Node n1, Node n2) {
        return cn.getAllEdges(n1, n2);
    }


    public ConstraintNetwork getConstraintNetwork() {
        return cn;
    }

    public EufLattice getEufLattice() {
        return euf.getLattice();
    }

    public ExecDag getExecutionTree() {
        return tree;
    }

    public Edge addConnection(Node src, Node target, EdgeKind kind, int prio) throws EUFInconsistencyException {
        return cn.addConnection(src, target, kind, prio);
    }

    public void addConnections(Set<Edge> edges) {
        cn.addConnections(edges);
    }


    public List<Node> getParametersFor(Node n) {
        return cn.getParametersFor(n);
    }

    public Set<Node> vertexSet() {
        return cn.vertexSet();
    }

    public Set<Edge> edgeSet() {
        return cn.edgeSet();
    }


    public Set<Node> getConnectedInNodes(Node n) {
        return cn.getConnectedInNodes(n);
    }

    public Set<Node> getConnectedOutNodes(Node n) {
        return cn.getConnectedOutNodes(n);
    }

    public void removeEdge(Node src, Node dst) {
        this.cn.removeEdge(src, dst);
    }

    public boolean removeVertex(Node n) {
        boolean ret = cn.removeVertex(n);
        listeners.forEach(e -> e.onNodeDelete(n));
        return ret;
    }

    public boolean containsVertex(Node n) {
        return cn.containsVertex(n);
    }


    public Set<Edge> incomingEdgesOf(Node n) {
        return cn.incomingEdgesOf(n);
    }

    public Set<Edge> outgoingEdgesOf(Node n) {
        return cn.outgoingEdgesOf(n);
    }

    public ConstraintNetworkBuilder clone() {
        return new ConstraintNetworkBuilder(this);
    }

    public void setStartNode(Node n) {
        cn.setStartNode(n);
    }

    public int outDegreeOf(Node n) {
        return cn.outDegreeOf(n);
    }

    public int inDegreeOf(Node n) {
        return cn.inDegreeOf(n);
    }

    public Node registerExtOperation(String bytecodesig, String lbl) {
        return cn.registerExtOperation(bytecodesig, lbl);
    }

    public void join(NodeKind kind, Node cpoint, ConstraintNetworkBuilder
            othercn) {
        cn.join(kind, cpoint, othercn.getConstraintNetwork());
    }


    public ConstraintNetworkBuilder subgraph(Collection<Node> vertices) {
        ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder(this);
        //cb.subgraph(vertices);
        cb.cn = this.cn.subgraph(vertices);
        return cb;
    }

    public Collection<Node> getAllVariables() {
        return cn.getAllVariables();
    }


    public Node merge(Node toReplace, Node replacement) throws
            EUFInconsistencyException {

        replacement.setNote(toReplace.getNote());
        assert !toReplace.equals(replacement);
        //LOGGER.debug("merge {}:{} and {}:{}", toReplace, toReplace.getId(),
        //        replacement, replacement.getId());


        if (!cn.containsVertex(toReplace) && cn.containsVertex(replacement)) {
            //assert false;

            LOGGER.debug("return");
            return replacement;
        }

        int id = toReplace.getId();

        assert cn.containsVertex(toReplace);

        Set<Edge> out = cn.outgoingEdgesOf(toReplace);
        //Set<Edge> in = cn.incomingEdgesOf(toReplace);

        Set<Edge> toAdd = new HashSet<>();

        for (Edge e : out) {
            assert !replacement.equals(e.getTarget());
            toAdd.add(new Edge(replacement, e.getTarget(), e.getSequence()));
        }

        //for(Edge e : in) {
        //    toAdd.add(new Edge(e.getSource(),replacement, e.getSequence()));
        //}

        assert containsVertex(toReplace);

        LOGGER.debug("-- remove {}", toReplace);
        addConnections(toAdd);
        removeVertex(toReplace);

        assert vertexSet().stream().filter(x -> x
                .getId() == id).count() == 0;


        if (!ConsistencyCheckerFactory.INSTANCE.isConsistent(this,
                replacement)) {
            throw new EUFInconsistencyException("malformed operand " + replacement
                    .getId());
        }

        NodeDomain isect = toReplace.getDomain().intersect(replacement
                .getDomain
                        ());

        LOGGER.debug("merge {} and {}", toReplace.getId(), replacement.getId());
        LOGGER.debug("ISECT {}" + isect.toString());

        if (isect == null || isect.isEmpty()) {
            throw new EUFInconsistencyException("could not merge " +
                    replacement.getId() + " and " + toReplace.getId());
        }
        replacement.setDomain(isect);

        if(replacement.getRange().isEmpty()) {

            throw new EUFInconsistencyException("could not merge " +
                    replacement.getId() + " and " + toReplace.getId());
        }

        // send a signal to all listeners
        listeners.forEach(e -> e.onNodeMerge(toReplace, replacement));

        if (!ConsistencyCheckerFactory.INSTANCE.isConsistent(this,
                replacement)) {
            throw new EUFInconsistencyException("malformed operand " + replacement
                    .getId());
        }


        return replacement;
    }

    private Node getCorrespondingNode(EquiClass c, Node n) {

        Predicate<Element> f;
        if (n.isOperation()) {
            f = e -> e.getAnnotation().equals(n.getLabel());
        } else {
            f = e -> e.getLabel().equals(n.getLabel());
        }

        return c.getElements().stream().filter(f).filter(e -> cn
                .containsVertex(e
                        .getMappedNode())).findFirst().get().getMappedNode();

    }

    // @TODO: make inference work properly for operands as well -- we need
    // to ensure that the returned nodes are definetely present in the CN
    public Node inferEquivalentNode(Node n) throws EUFInconsistencyException {

        LOGGER.debug("infer {}:{}", n.getLabel(),n.getId());
        //LOGGER.debug(getEufLattice().toDot());
        // equiclass which is present
        //EquiClass nn = null;
        EquiClass nen = euf.inferActualEquiClassForNode(n);

        EquiClass nn = euf.getEquiClassForNode(n);

        //LOGGER.debug("actual {}:{}", nen.getLabel(), nen.getId());
        //LOGGER.debug("new {}:{}", nn.getLabel(), nn.getId());

        assert nen != null;
        assert nen != euf.getBottom();
        //assert nen != euf.getTop();

        if (nen.equals(euf.getTop()))
            return n;

        //LOGGER.debug(getEufLattice().toDot());
        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

        Element cor = nen.getCorrespondingElement(n);

        if(cor == null)
            return n;

        //Node emap = getCorrespondingNode(nn,n);
        Node emap = cor.getMappedNode();


        //LOGGER.debug("mapped node {}:{}", nen.getDotLabel(),nen.getId());
        //assert cn.containsVertex(emap);

        // whenever we infer a new fact it will be added to our euf
        // lattice
        if (!emap.equals(n)) {
            EquiClass nemap = nen.union(nn);
            try {
                nn = euf.addEquiClass(nemap);
            } catch (EUFInconsistencyException e) {
                assert false;
            }
            return nn.getCorrespondingElement(n).getMappedNode();
        }
        return emap;
    }


}

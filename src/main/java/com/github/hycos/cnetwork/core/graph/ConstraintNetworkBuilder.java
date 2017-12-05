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


import com.github.hycos.cnetwork.api.NodeInterface;
import com.github.hycos.cnetwork.api.NodeKindInterface;
import com.github.hycos.cnetwork.api.cchecktinf.ConsistencyCheckerInterface;
import com.github.hycos.domctrl.DomainControllerInterface;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkInterface;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkListenerInterface;
import com.github.hycos.cnetwork.api.labelmgr.LabelManagerInterface;
import com.github.hycos.cnetwork.api.labelmgr.LabelManagerListenerInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.cchecktinf.DefaultConsistencyChecker;
import com.github.hycos.cnetwork.core.DefaultDomainController;
import com.github.hycos.cnetwork.core.DefaultLabelManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;


public class ConstraintNetworkBuilder implements Cloneable,
        ConstraintNetworkInterface<Node>, LabelManagerListenerInterface {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkBuilder.class);

    private DomainControllerInterface<Node> dctrl;
    private LabelManagerInterface<Node> lmgr;
    private ConsistencyCheckerInterface<Node> ci;

    private ConstraintNetwork cn;

    // observer structural changes to on the cn
    private Set<ConstraintNetworkListenerInterface<Node>> listeners = new
            LinkedHashSet<>();

    // Note that listeners are no copied
    public ConstraintNetworkBuilder(ConstraintNetworkBuilder cnb) {
        this.cn = new ConstraintNetwork(cnb.cn);
        Objects.requireNonNull(cnb.lmgr);
        Objects.requireNonNull(cnb.dctrl);
        this.dctrl = cnb.dctrl.clone();
        this.lmgr = cnb.lmgr.clone();
        this.ci = cnb.ci.clone();
        // rewire newly created objects
        registerListeners(this.dctrl, this.lmgr);

        for (Node n : vertexSet()) {
            n.setLabelManager(this.lmgr);
            n.setDomainController(this.dctrl);
        }
        attachObservers();
    }

    public ConstraintNetworkBuilder() {
        this(new DefaultDomainController(),
                new DefaultLabelManager(),
                new DefaultConsistencyChecker());
        //registerListeners(this.dctrl, this.lmgr);
        //this.cn = new ConstraintNetwork();
        //registerListeners(dctrl, lmgr);
        //this.lmgr = new EufManager(this);
        //this.listeners.add(this.lmgr);

    }

    public ConstraintNetworkBuilder
            (DomainControllerInterface<Node> dctrl,
             LabelManagerInterface<Node> lmgr,
             ConsistencyCheckerInterface<Node> ci) {
        this.cn = new ConstraintNetwork();

        // domain controller has to be set first always
        this.dctrl = dctrl;

        this.lmgr = lmgr;
        this.ci = ci;

        registerListeners(this.dctrl, this.lmgr);

        LOGGER.debug("set dctrl {}", this.dctrl.getClass().getName());
        LOGGER.debug("set lmgr {}", this.lmgr.getClass().getName());
        //this.lmgr = new EufManager(this);
        //this.listeners.add(this.lmgr);
    }


    public void registerListeners(ConstraintNetworkListenerInterface<Node>... l) {
        LOGGER.debug("register {}", l.length);
        listeners.addAll(Arrays.asList(l));
        listeners.forEach(x -> x.register(this));
    }

    private void attachObservers() {
        cn.vertexSet().forEach(v -> attachObservers(v));
    }

    private void attachObservers(Node n) {
        this.listeners.forEach(o -> n.attach(o));
    }

    private void updateObservers(Node n) throws InconsistencyException {

        //LOGGER.debug("UPATE SIZ: " + n.getDomain().size());
        //assert n.getDomain().size() == 2;

        for (ConstraintNetworkListenerInterface o : listeners) {
            o.update(n);
        }
    }

    public Node getNodeById(int id) {
        return this.cn.getNodeById(id);
    }

    public Node getNodeByLabel(String lbl) {
        LOGGER.debug("get node by label {}", lbl);
        return lmgr.getNodeByLabel(lbl);
    }

    public boolean hasNodeForLabel(String lbl) {
        return this.lmgr.hasNodeForLabel(lbl);
    }

    public String getLabelForNode(Node n) {
        return this.lmgr.getLabelForNode(n);
    }

    public Node addConstraint(NodeKindInterface kind, Node... params) throws
            InconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addConstraint(kind, lst);
    }

    public Node addOperation(NodeKindInterface kind, Node... params) throws InconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addOperation(kind, lst);
    }

    public Edge addConnection(Edge e) throws InconsistencyException {
        Edge ne = cn.addConnection(e);
        listeners.forEach(l -> l.onConnectionAdd(e.getSource(), e.getTarget()));
        return ne;
    }


    public void addListener(ConstraintNetworkListenerInterface<Node> listener) {
        this.listeners.add(listener);
    }

    public void addListener(Set<ConstraintNetworkListenerInterface<Node>>
                                    listeners) {
        this.listeners.addAll(listeners);
    }


    @Override
    public Node addOperand(NodeKindInterface n, String s) {

        Node op = cn.addOperand(n, s);
        LOGGER.debug("add operand {}:{}", op.getKind(), s);

        for (ConstraintNetworkListenerInterface<Node> l : listeners) {
            try {
                l.onNodeAdd(op,false);
            } catch (InconsistencyException e) {
                // Should never ever happen
                assert false;
            }
        }

        //LOGGER.debug("lbl for node {}:{}", op.getId(), op.getLabel());
        try {
            Node nop = infer(op);

            if(!nop.equals(op))
                removeVertex(op);

            return nop;
        } catch (InconsistencyException e) {
            LOGGER.error(e.getMessage());
            e.printStackTrace();
            assert false;
        }

        return null;
    }

    @Override
    public void addConnection(Node frst, Node snd, int prio) throws InconsistencyException {
        addConnection(frst, snd, EdgeKind.PAR_IN, prio);
    }

    @Override
    public boolean checkConsistency() {
        for(Node n : vertexSet()) {
            if(!this.ci.check(this, n)) {
                return false;
            }
        }
        return true;
    }

    private Node infer(Node n) throws InconsistencyException {

        Node nop = (Node) lmgr.infer(n);

        if (n.equals(nop)) {
            addVertex(nop);
        }

        LOGGER.debug("infer {}", nop.getDotLabel());

        if (!this.ci.check(this, nop)) {
            throw new InconsistencyException("malformed operand " + nop
                    .getKind());
        }

        if (n.equals(nop)) {
            attachObservers(nop);
            updateObservers(nop);
            lmgr.attach(nop);
            lmgr.update(nop);
            //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

            return nop;
        } else {

            //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);
            //LOGGER.debug("BOOOOO {} : {}", n.getId(), n.getDomain().getLabel
            //        ());
            //cn.removeVertex(n);
            //LOGGER.debug("BOOOOO {} : {}", nop.getId(), nop.getDomain()
            //        .getLabel
            //        ());
            //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

            return nop;
        }
    }

    public List<Node> inferParams(List<Node> params) throws
            InconsistencyException {
        List<Node> plist = new Vector<>();
        LOGGER.debug("infer params");
        for (Node p : params) {

            Node i = infer(p);

            LOGGER.debug("inferred {}:{}:{}", p.getId(), i.getId(), i
                    .getDomain().toString());
            plist.add(i);
        }

        assert plist.size() == params.size();
        return plist;
    }

    public Node addOperation(NodeKindInterface kind, List<Node> params) throws InconsistencyException {
        return addOperation(kind, params, false);
    }

    private Node addOperation(NodeKindInterface kind, List<Node> params,
                              boolean isConstraint)
            throws
            InconsistencyException {


        Node op = cn.addOperation(kind, inferParams(params));

        for (ConstraintNetworkListenerInterface l : listeners) {
            l.onNodeAdd(op,isConstraint);
        }


        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);
        LOGGER.debug("check node {}:{}", op.getShortLabel(), op.getId());

        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

        //if(op.getId() == 233)
        //    System.exit(-1);
        Node nop = infer(op);

        LOGGER.debug("nop {}", nop.getId());
        LOGGER.debug("op {}", op.getId());

        // there is a node with a differnt
        if (nop.getId() != op.getId()) {
            LOGGER.debug("** remove {}", op.getId());
            //cn.removeVertex(op);
            removeVertex(op);
        }


        LOGGER.debug("CN {}:{}", nop.getId(), nop.getDomain().toString());

        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);

        return nop;
    }


    public Node addConstraint(NodeKindInterface kind, List<Node> params) throws
            InconsistencyException {

        Node op = addOperation(kind, params, true);

        return op;
    }

    public Operation addExtOperation(String identifier, List<Node> params) {
        return cn.addExtOperation(identifier, params);
    }

    public boolean removeAllVertices(Collection<? extends Node> n) {

        for (Node nd : n) {
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


    public Edge addConnection(Node src, Node target, EdgeKind kind, int prio) throws InconsistencyException {
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

    public Node addVertex(Node n) throws InconsistencyException {
        Node v = cn.addNode(n);
        for (ConstraintNetworkListenerInterface<Node> l : listeners) {
            l.onNodeAdd(n, false);
        }
        return v;
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

    public Node registerExtOperation(String bytecodesig, NodeKindInterface ni,
            String
                                     lbl) {

        return cn.registerExtOperation(bytecodesig, ni, lbl);
    }

    public void join(NodeKindInterface kind, Node cpoint, ConstraintNetworkBuilder
            othercn) throws InconsistencyException {

        for (Node n : othercn.vertexSet()) {

            if(n.isConstraint()) {
                Node v = addVertex(n);
                v.getDomain().setTrue();
            } else {
                addVertex(n);
            }
        }

        for (Edge e : othercn.edgeSet()) {
            addConnection(e);
        }

        addConstraint(kind, cpoint, othercn.getConstraintNetwork()
                .getStartNode());

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


    public Node collapse(Node toReplace, Node replacement) throws
            InconsistencyException {

        replacement.setNote(toReplace.getNote());
        assert !toReplace.equals(replacement);
        //LOGGER.debug("collapse {}:{} and {}:{}", toReplace, toReplace.getId(),
        //        replacement, replacement.getId());


        if (!cn.containsVertex(toReplace) && cn.containsVertex(replacement)) {
            //assert false;

            LOGGER.debug("return");
            return replacement;
        }
        assert cn.containsVertex(toReplace);

        Set<Edge> out = cn.outgoingEdgesOf(toReplace);
        //Set<Edge> in = cn.incomingEdgesOf(toReplace);

        Set<Edge> toAdd = new HashSet<>();

        for (Edge e : out) {
            //assert !replacement.equals(e.getTarget());
            toAdd.add(new Edge(replacement, e.getTarget(), e.getSequence()));
        }

        //for(Edge e : in) {
        //    toAdd.add(new Edge(e.getSource(),replacement, e.getSequence()));
        //}

        assert containsVertex(toReplace);

        LOGGER.debug("-- remove {}", toReplace);
//        addConnections(toAdd);
//        removeVertex(toReplace);
//
//        assert vertexSet().stream().filter(x -> x
//                .getId() == id).count() == 0;


        if (!this.ci.check(this, replacement)) {
            throw new InconsistencyException("malformed operand " + replacement
                    .getId());
        }

        for (ConstraintNetworkListenerInterface lst : listeners) {
            lst.onNodeCollapse(toReplace, replacement);
        }

        addConnections(toAdd);
        removeVertex(toReplace);

        if (!this.ci.check(this, replacement)) {
            throw new InconsistencyException("malformed operand " + replacement
                    .getId());
        }


        return replacement;
    }

    @Override
    public void onEquivalentNodeLabels(NodeInterface toReplace, NodeInterface
            replacement) throws InconsistencyException {
        collapse((Node) toReplace, (Node) replacement);
    }


}



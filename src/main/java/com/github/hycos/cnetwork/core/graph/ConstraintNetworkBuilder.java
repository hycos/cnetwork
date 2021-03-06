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


import com.github.hycos.cnetwork.api.EdgeInterface;
import com.github.hycos.cnetwork.api.NodeInterface;
import com.github.hycos.cnetwork.api.NodeKindInterface;
import com.github.hycos.cnetwork.api.cchecktinf.ConsistencyCheckerInterface;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkInterface;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkListenerInterface;
import com.github.hycos.cnetwork.api.labelmgr.LabelManagerInterface;
import com.github.hycos.cnetwork.api.labelmgr.LabelManagerListenerInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.cchecktinf.DefaultConsistencyChecker;
import com.github.hycos.cnetwork.core.DefaultDomainController;
import com.github.hycos.cnetwork.core.DefaultLabelManager;
import com.github.hycos.cnetwork.core.Enumerator;
import com.github.hycos.cnetwork.sig.JavaMethodSignature;
import com.github.hycos.cnetwork.utils.Pair;
import com.github.hycos.domctrl.DomainControllerInterface;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;


public class ConstraintNetworkBuilder implements Cloneable,
        ConstraintNetworkInterface<Node>,
        LabelManagerListenerInterface {

    private static final long serialVersionUID = -8834622791197111310L;

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkBuilder.class);

    private DomainControllerInterface<Node, Edge> dctrl;
    private LabelManagerInterface<Node, Edge> lmgr;
    private ConsistencyCheckerInterface<Node, Edge> ci;
    private ConstraintNetworkListenerInterface<Node, Edge> en;

    private ConstraintNetwork cn;

    // observer structural changes to on the cn
    private List<ConstraintNetworkListenerInterface<Node, Edge>> listeners =
            new Vector<>();


    private HashMap<String, Operation> opLookup = new HashMap<>();


    public ConstraintNetworkBuilder(ConstraintNetworkBuilder cnb) {
        this.cn = new ConstraintNetwork(cnb.cn);
        Objects.requireNonNull(cnb.lmgr);
        Objects.requireNonNull(cnb.dctrl);
        this.dctrl = cnb.dctrl.clone();
        this.lmgr = cnb.lmgr.clone();
        this.ci = cnb.ci.clone();
        this.en = cnb.en.clone();
        // rewire newly created objects
        registerListeners(this.en, this.dctrl, this.lmgr);

        for (Node n : vertexSet()) {
            n.setLabelManager(this.lmgr);
            n.setDomainController(this.dctrl);
        }
        attachObservers();
    }


    public LabelManagerInterface<Node, Edge> getLabelManager() {
        return this.lmgr;
    }

    public ConstraintNetworkBuilder() {
        this(new Enumerator(),
                new DefaultDomainController(),
                new DefaultLabelManager(),
                new DefaultConsistencyChecker());
    }



    public ConstraintNetworkBuilder
            (ConstraintNetworkListenerInterface<Node, Edge> enumerator,
             DomainControllerInterface<Node, Edge> dctrl,
             LabelManagerInterface<Node, Edge> lmgr,
             ConsistencyCheckerInterface<Node, Edge> ci) {
        this.cn = new ConstraintNetwork();

        // domain controller has to be set first always
        this.dctrl = dctrl;
        this.lmgr = lmgr;
        this.ci = ci;
        this.en = enumerator;

        registerListeners(this.en,this.dctrl, this.lmgr);

        LOGGER.debug("set dctrl {}", this.dctrl.getClass().getName());
        LOGGER.debug("set lmgr {}", this.lmgr.getClass().getName());
    }


    public void registerListeners(ConstraintNetworkListenerInterface<Node, Edge>... l) {
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

    public Node addOperation(NodeKindInterface kind, Node... params)
            throws InconsistencyException {
        List<Node> lst = Arrays.asList(params);
        return addOperation(kind, lst);
    }

    public Edge addConnection(Edge e) throws InconsistencyException {
        Edge ne = cn.addConnection(e);
        listeners.forEach(l -> l.onConnectionAdd(e.getSource(), e.getTarget(),e));
        return ne;
    }

    @Override
    public void addConnection(Node frst, Node snd, int prio) throws InconsistencyException {
        addConnection(frst, snd, EdgeKind.PAR_IN, prio);
    }

    public Operation registerExtOperation(String bytecodesig,
                                             NodeKindInterface ki,  String
                                                     label) {

        //LOGGER.info("bytecodesig " + bytecodesig);
        //LOGGER.info("name " + label);

        if (opLookup.containsKey(bytecodesig))
            return null;

        JavaMethodSignature sig = JavaMethodSignature.fromString(bytecodesig);

        Operation op = new Operation(label, ki, sig);
        opLookup.put(label, op);


        //LOGGER.info("OPLOOKUP " + op.toString());

        return op;
    }


    public Edge createConnection(Node src, Node target, EdgeKind kind,
                                 int prio) {
        Edge e = new Edge(src, target, kind, prio);
        listeners.forEach(l -> l.beforeConnectionAdd(e.getSource(), e.getTarget(),e));
        return e;
    }

    public Edge addConnection(Node src, Node target, EdgeKind kind, int prio)
            throws InconsistencyException {
        return cn.addConnection(createConnection(src,target,kind,prio));
    }


    public void addListener(ConstraintNetworkListenerInterface<Node, Edge> listener) {
        this.listeners.add(listener);
    }

    public void addListener(Set<ConstraintNetworkListenerInterface<Node, Edge>>
                                    listeners) {
        this.listeners.addAll(listeners);
    }


    @Override
    public Node addOperand(NodeKindInterface n, String s) {

        Node op = new Operand(s,n);

        for (ConstraintNetworkListenerInterface l : listeners) {
            try {
                l.beforeNodeAdd(op);
            } catch (InconsistencyException e) {
                assert false;
            }
        }


        //cn.addNode(op);


        LOGGER.debug("add operand {}:{}", op.getKind(), s);

        for (ConstraintNetworkListenerInterface<Node, Edge> l : listeners) {
            try {
                l.onNodeAdd(op, false);
            } catch (InconsistencyException e) {
                // Should never ever happen
                assert false;
            }
        }

        //LOGGER.debug("lbl for node {}:{}", op.getId(), op.getLabel());
        try {
            Node nop = infer(op);

            if (!nop.equals(op))
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
    public boolean checkConsistency() {
        for (Node n : vertexSet()) {
            if (!this.ci.check(this, n)) {
                return false;
            }
        }
        return true;
    }

    protected Operation getExtOperation(String label) {
        if (!this.opLookup.containsKey(label))
            return null;
        return this.opLookup.get(label);
    }



    protected Node addExtOperation(String identifier, List<Node> params) throws InconsistencyException {

        Operation ext = getExtOperation(identifier);

        if (ext == null)
            return null;


        Pair<Node, Set<Edge>> nop = createOperation(ext, params);


        return linkParamsToOp(nop, false);
    }




    private Node infer(Node n) throws InconsistencyException {

        Node nop = (Node) lmgr.infer(n);

        if (n.equals(nop)) {
            addVertex(nop, nop.isConstraint());
        }

        LOGGER.debug("infer {}", nop.getDotLabel());

        LOGGER.debug(this.getConstraintNetwork().toDot());

        if (!this.ci.check(this, nop)) {
            throw new InconsistencyException("malformed operand " + nop.getKind());
        }

        if (n.equals(nop)) {
            attachObservers(nop);
            updateObservers(nop);
            lmgr.attach(nop);
            lmgr.update(nop);
            return nop;
        } else {
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


    private Set<Edge> createParameterLinks(Node op, List<Node> params) {

        Set<Edge> ret = new HashSet<>();

        for (int i = 0; i < params.size(); i++) {
            Node par = params.get(i);
            ret.add(createConnection(par, op, EdgeKind.PAR_IN, i));
        }

        return ret;
    }


    public Node addOperation(NodeKindInterface kind, List<Node> params)
            throws InconsistencyException {
        return addOperation(kind, params, false);
    }


    public Pair<Node, Set<Edge>> createOperation(NodeKindInterface kind,
                                                 List<Node> params) {
        Node op = new Operation(kind);
        Set<Edge> links = createParameterLinks(op, params);
        return new Pair(op, links);
    }


    public Pair<Node, Set<Edge>> createOperation(Operation op,
                                                 List<Node> params) {
        Set<Edge> links = createParameterLinks(op, params);
        return new Pair(op, links);
    }



    private Node linkParamsToOp(Pair<Node, Set<Edge>> ops, boolean isConstraint) throws InconsistencyException {


        Node op = ops.getFirst();
        Set<Edge> links = ops.getSecond();

        for (ConstraintNetworkListenerInterface l : listeners) {
            l.beforeNodeAdd(op);
        }

        LOGGER.debug("lbl {}", op.getLabel());

        cn.addNode(op);

        for (ConstraintNetworkListenerInterface l : listeners) {
            l.onNodeAdd(op, isConstraint);
        }


        addConnections(links);

        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);
        LOGGER.debug("check node {}:{}", op.getShortLabel(), op.getId());

        //assert ConsistencyCheckerFactory.INSTANCE.checkConsistency(this);
        Node nop = infer(op);

        LOGGER.debug("nop {}", nop.getId());
        LOGGER.debug("op {}", op.getId());

        // there is a node with a differnt
        if (nop.getId() != op.getId()) {
            LOGGER.debug("** remove {}", op.getId());
            //cn.removeVertex(op);
            removeVertex(op);
        }

        return nop;

    }


    private Node addOperation(NodeKindInterface kind, List<Node> params, boolean isConstraint)
            throws
            InconsistencyException {

        Pair<Node, Set<Edge>> ops = createOperation(kind, inferParams(params));

        return linkParamsToOp(ops, isConstraint);
    }


    public Node addConstraint(NodeKindInterface kind, List<Node> params) throws
            InconsistencyException {

        Node op = addOperation(kind, params, true);

        return op;
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


    public void addConnections(Set<Edge> edges) {
        for(Edge e : edges) {
            for (ConstraintNetworkListenerInterface l : listeners) {
                l.beforeConnectionAdd(e.getSource(),e.getTarget(),e);
            }

            cn.addConnection(e);

            for (ConstraintNetworkListenerInterface l : listeners) {
                l.onConnectionAdd(e.getSource(),e.getTarget(),e);
            }
        }
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


    private Node addVertex(Node n, boolean isConstraint) throws InconsistencyException {
        for (ConstraintNetworkListenerInterface<Node, ? extends EdgeInterface> l : listeners) {
            l.beforeNodeAdd(n);
        }
        Node v = cn.addNode(n);
        for (ConstraintNetworkListenerInterface<Node, ? extends EdgeInterface> l : listeners) {
            l.onNodeAdd(n, isConstraint);
        }
        return v;
    }

    public Node addNode(Node n , boolean isConstraint) throws InconsistencyException {
        return addVertex(n, isConstraint);
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


    public void join(NodeKindInterface kind, Node cpoint, ConstraintNetworkBuilder othercn) throws InconsistencyException {

        Map<Node, Node> nmap = new HashMap<>();

        for (Node n : othercn.vertexSet()) {

            Node v = n.clone(); v.id = -1; // reset id to default so that the
            // enumerator can assign a new value to it
            //v.cn = this.getConstraintNetwork();
            //v.id = this.getConstraintNetwork().nextNodeId();
//

            addVertex(v, n.isConstraint());

            if (n.isConstraint())
                v.getDomain().setTrue();
//
            nmap.put(n, v);
        }

        for (Edge e : othercn.edgeSet()) {

            Node src = nmap.get(e.getSource());
            Node dst = nmap.get(e.getDestNode());

            Edge ne = new Edge(src, dst, e.getKind(), e.getSequence());

            addConnection(ne);
        }

        addConstraint(kind, cpoint, othercn.getConstraintNetwork().getStartNode());

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

    private Object writeReplace()
            throws java.io.ObjectStreamException {
        LOGGER.debug("write replace");

        LOGGER.debug("this");

        ConstraintNetworkBuilder cp = new ConstraintNetworkBuilder(this);

        LOGGER.debug(cp.toString());

        return cp;
    }

}



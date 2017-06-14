package org.snt.cnetwork.core.execdag;

/**
 * Created by julian on 26/04/2017.
 */

import org.jgrapht.experimental.dag.DirectedAcyclicGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.graph.*;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.utils.EscapeUtils;

import java.util.Collection;
import java.util.List;
import java.util.Set;


/**
 * a more lightweight representation of a constraint network which
 * can be used in order to apply certain heuristics
 */
public class ExecDag extends DirectedAcyclicGraph<Node,
        ExecEdge> implements ConstraintNetworkObserver<Node>,
        ConstraintNetworkEventListener {

    final static Logger LOGGER = LoggerFactory.getLogger(ExecDag.class);

    private ConstraintNetworkBuilder cn = null;

    public ExecDag() {
        super(ExecEdge.class);
    }

    public ExecDag(ConstraintNetworkBuilder cn) {
        this();
        this.cn = cn;
    }

    public ExecDag(ExecDag t, ConstraintNetworkBuilder cn) {
        this(cn);
        addAllVertices(cn.vertexSet());
        addAllEdges(t.edgeSet());
        this.cn = t.cn;
    }

    public void addAllVertices(Collection<Node> n) {
        n.stream().forEach(v -> addVertex(v));
    }

    public void addAllEdges(Collection<ExecEdge> edges) {
        try {
            for (ExecEdge e : edges) {
                ExecEdge ne = new ExecEdge(e.getSource(), e.getTarget(), e
                        .getSequence());
                addDagEdge(e.getSource(), e.getTarget(), ne);
            }
        } catch (CycleFoundException e1) {
            assert false;
        }
    }

    @Override
    public boolean addVertex(Node n) {

        if (containsVertex(n))
            return false;
        else super.addVertex(n);

        if (n.isOperation()) {
            List<Node> pars = cn.getParametersFor(n);
            for (Node p : pars) {
                addVertex(p);
                addExecEdge(n, p, pars.indexOf(p));
            }
        }

        return true;
    }

    private void addExecEdge(Node src, Node dest, int seq) {
        ExecEdge e = new ExecEdge(src, dest, seq);
        try {
            addDagEdge(src, dest, e);
        } catch (CycleFoundException e1) {
            LOGGER.error("cycle detected");
            assert false;
        }
    }


    public String toDot() {

        StringBuilder sb = new StringBuilder();

        sb.append("digraph {\n" +
                "\trankdir=TB;\n");

        sb.append("\tnode [fontname=Helvetica,fontsize=11];\n");
        sb.append("\tedge [fontname=Helvetica,fontsize=10];\n");

        String shape;
        String label;
        String color;

        for (Node n : this.vertexSet()) {

            if (!cn.containsVertex(n))
                continue;

            shape = "box";
            label = "label";
            color = "black";

            if (n.isConstraint())
                color = "orange";
            else if (n.isOperation())
                color = "gray";

            if (n.isOperand())
                shape = "ellipse";


            sb.append("\tn" + n.getId() + " [color=" + color + ",shape=\"" +
                    shape + "\"," + label + "=\"" + EscapeUtils
                    .escapeSpecialCharacters(n.getLabel()) + "[" + n.getId()
                    + "]\"];\n");
        }


        for (ExecEdge e : this.edgeSet()) {
            Node src = e.getSource();
            Node dest = e.getTarget();

            if (!cn.containsVertex(src) || !cn.containsVertex(dest))
                continue;

            assert src != null;
            assert dest != null;

            sb.append("\tn" + src.getId() + " -> n" + dest.getId() +
                    "[label=\"" + e.getSequence() + "\"];\n");

        }
        sb.append("}");
        return sb.toString();
    }


    @Override
    public void update(Node n) throws EUFInconsistencyException {
        if (n.isConstraint()) {
            addVertex(n);
        }
    }

    @Override
    public void attach(Node n) {
        n.attach(this);
    }

    @Override
    public void onNodeMerge(Node toReplace, Node replacement) {

        if(!containsVertex(toReplace)) {
            addVertex(replacement);
            return;
        }

        Set<ExecEdge> in = incomingEdgesOf(toReplace);
        removeVertex(toReplace);
        addVertex(replacement);

        for(ExecEdge i : in) {
            addExecEdge(i.getSource(), replacement, i.getSequence());
        }

    }

    @Override
    public void onNodeDelete(Node deleted) {
        removeVertex(deleted);
    }

    @Override
    public void onAddConnection(Edge e) {

    }

    public void inferFacts() {

    }
}

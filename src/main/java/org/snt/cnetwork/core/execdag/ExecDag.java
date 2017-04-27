package org.snt.cnetwork.core.execdag;

/**
 * Created by julian on 26/04/2017.
 */

import org.jgrapht.experimental.dag.DirectedAcyclicGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.graph.ConstraintNetworkObserver;
import org.snt.cnetwork.core.graph.Node;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetwork.utils.EscapeUtils;

import java.util.Collection;
import java.util.List;


/**
 * a more lightweight representation of a constraint network which
 * can be used in order to apply certain heuristics
 */
public class ExecDag extends DirectedAcyclicGraph<Node,
        ExecEdge> implements ConstraintNetworkObserver<Node> {

    final static Logger LOGGER = LoggerFactory.getLogger(ExecDag.class);

    private ConstraintNetworkBuilder cn = null;

    public ExecDag() {
        super(new ExecEdgeFact());
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
                addDagEdge(e.getSource(), e.getTarget());
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
                try {
                    addVertex(p);
                    addDagEdge(n, p);
                } catch (CycleFoundException e) {
                    LOGGER.error("cycle detected");
                    assert false;
                }
            }
        }

        return true;
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

        for (Node n : this.vertexSet()) {
            String kind = "";

            shape = "box";
            label = "label";
            color = "black";


            sb.append("\tn" + n.getId() + " [color=" + color + ",shape=\"" +
                    shape + "\"," + label + "=\"" + EscapeUtils
                    .escapeSpecialCharacters(n.getLabel()) +
                    "\"];\n");
        }

        String option = "";
        String ecolor = "black";
        String par = "";

        //LOGGER.debug("eset {}", edgeSet().size());

        for (ExecEdge e : this.edgeSet()) {
            Node src = e.getSource();
            Node dest = e.getTarget();

            //LOGGER.debug("src {}", src.getDotLabel());

            //assert outgoingEdgesOf(src).contains(e);
            //assert incomingEdgesOf(dest).contains(e);

            assert src != null;
            assert dest != null;

            ecolor = "black";

            par = "";


            sb.append("\tn" + src.getId() + " -> n" + dest.getId() +
                    "[color=\"" + ecolor + "\"];\n");

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
}

package org.snt.cnetwork.preproc;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Edge;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;

import java.util.List;

public class DisjunctionTranslator implements Converter {

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkPreprocessor.class);


    @Override
    public NodeKind getKind() {
        return NodeKind.OR;
    }

    /**
     * Translate disjunction into conjunction (De Morgan)
     * @param cn
     * @return
     */
    @Override
    public void translate(ConstraintNetwork cn, Node n) {

        LOGGER.info(n.getLabel());
        assert n.getKind() == NodeKind.OR;
        List<Node> pars = cn.getParametersFor(n);
        LOGGER.debug("pars " +  pars.size());

        assert pars.size() == 2;
        Node par0 = pars.get(0);
        Node par1 = pars.get(1);
        Node npar0 = cn.addOperation(NodeKind.NOT, par0);
        Node npar1 = cn.addOperation(NodeKind.NOT, par1);
        Node and = cn.addOperation(NodeKind.AND, npar0, npar1);
        Node nand = cn.addOperation(NodeKind.NOT, and);
        nand.setDomain(n.getDomain().clone());
        for(Edge e : cn.outgoingEdgesOf(n)) {
            Edge ne = new Edge(nand, e.getDestNode(),e.getSequence());
            cn.addConnection(ne);
        }
        cn.removeVertex(n);
    }



    /**@Override
    public Collection<ConstraintNetwork> translate(ConstraintNetwork cn) {

        Set<ConstraintNetwork> ret = new HashSet();
        LinkedList<ConstraintNetwork> worklist = new LinkedList<>();
        worklist.add(cn);

        while (!worklist.isEmpty()) {

            ConstraintNetwork nextNetwork = worklist.poll();

            assert !nextNetwork.vertexSet().isEmpty();

            Set<Node> disjunctions = nextNetwork.vertexSet()
                    .stream()
                    .filter(v -> v
                            .getKind() == NodeKind.OR).collect(Collectors.toSet());


            if (disjunctions == null || disjunctions.isEmpty()) {
                ret.add(nextNetwork);
                continue;
            }

            // there are still disjunctions present -- resolve one at a time
            Node dis = disjunctions.iterator().next();

            List<Node> disparams = nextNetwork.getParametersFor(dis);
            Set<Node> disout = nextNetwork.getConnectedOutNodes(dis);

            // remove disjunction
            nextNetwork.removeVertex(dis);

            for (Node par : disparams) {
                for (Node out : disout) {
                    nextNetwork.addEdge(par, out);
                }
                par.setDomain(dis.getDomain().clone());
            }

            Node par0 = disparams.get(0);
            Node par1 = disparams.get(1);


            LOGGER.debug("first param of dis {}", par0.getId());
            LOGGER.debug("second param of dis {}", par1.getId());

            CnetworkSlicerBackward bw = new CnetworkSlicerBackward
                    (nextNetwork);


            // @TODO: make this a bit more efficient
            ConstraintNetwork cp1 = nextNetwork.clone();
            ConstraintNetwork cp2 = nextNetwork.clone();

            cp1.removeVertex(par0);
            cp2.removeVertex(par1);

            worklist.add(cp1);
            worklist.add(cp2);

        }
        return ret;
    }**/

}

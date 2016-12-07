package org.snt.cnetwork.utils;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.slicer.CnetworkSlicerBackward;

import java.util.*;
import java.util.stream.Collectors;

public enum CnetworkTool {

    INSTANCE;

    final static Logger LOGGER = LoggerFactory.getLogger(CnetworkTool.class);


    public Collection<ConstraintNetwork> removeDisjunctions(ConstraintNetwork
                                                                  cn) {
        return split(cn);
    }

    /**
     * A fixed point iteration that resolves one disjunction at time
     * @param cn constraint network
     * @return
     */
    private Collection<ConstraintNetwork> split(ConstraintNetwork cn) {

        Set<ConstraintNetwork> ret = new HashSet();
        LinkedList<ConstraintNetwork> worklist = new LinkedList<>();
        worklist.add(cn);

        while(!worklist.isEmpty()) {

            ConstraintNetwork nextNetwork = worklist.poll();
            assert !nextNetwork.vertexSet().isEmpty();

            Set<Node> disjunctions = nextNetwork.vertexSet()
                    .stream()
                    .filter(v -> v
                    .getKind() == NodeKind.OR).collect(Collectors.toSet());


            if(disjunctions == null || disjunctions.isEmpty()) {
                ret.add(nextNetwork);
                continue;
            }

            // there are still disjunctions present -- resolve one at a time
            Node dis = disjunctions.iterator().next();

            List<Node> disparams = nextNetwork.getParametersFor(dis);
            Set<Node> disout = nextNetwork.getConnectedOutNodes(dis);

            // remove disjunction
            nextNetwork.removeVertex(dis);

            for(Node out : disout) {
                for(Node par : disparams) {
                    nextNetwork.addEdge(par, out);
                    par.setDomain(dis.getDomain().clone());
                }
            }

            Node par0 = disparams.get(0);
            Node par1 = disparams.get(1);

            // clone the domain of the original element
            par0.setDomain(dis.getDomain().clone());
            par1.setDomain(dis.getDomain().clone());


            LOGGER.debug("first param of dis {}", par0.getId());
            LOGGER.debug("second param of dis {}", par1.getId());

            CnetworkSlicerBackward bw = new CnetworkSlicerBackward(nextNetwork);

            Collection<Node> par0bw = bw.slice(par0);
            Collection<Node> par1bw = bw.slice(par1);

            // get the intersection to find elemnents that are in both bw slices
            Set<Node> isect = new HashSet(par0bw); isect.retainAll(par1bw);

            par0bw.removeAll(isect);
            par1bw.removeAll(isect);

            // @TODO: make this a bit more efficient
            ConstraintNetwork cp1 = nextNetwork.clone();
            ConstraintNetwork cp2 = nextNetwork.clone();

            Set<Node> cp1v = new HashSet(cp1.vertexSet());
            Set<Node> cp2v = new HashSet(cp2.vertexSet());

            // remove the nodes that are no longer required
            cp1v.removeAll(par0bw);
            cp2v.removeAll(par1bw);

            ConstraintNetwork cp1r = cp1.subgraph(cp1v).clone();
            ConstraintNetwork cp2r = cp2.subgraph(cp2v).clone();

            worklist.add(cp1r);
            worklist.add(cp2r);
        }
        return ret;
    }
}

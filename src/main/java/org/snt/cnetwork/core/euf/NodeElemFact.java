package org.snt.cnetwork.core.euf;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.exception.MissingItemException;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Generate Equivalence Classes from Nodes
 */
public final class NodeElemFact implements EquiClassFact<Node> {

    final static Logger LOGGER = LoggerFactory.getLogger(NodeElemFact.class);

    private ConstraintNetworkBuilder cn;

    private NodeCache cache = new NodeCache();

    public NodeElemFact(ConstraintNetworkBuilder cn, NodeElemFact ne) {
        this(cn);
        // clone cache
        this.cache = new NodeCache(ne.cache);

    }

    public NodeElemFact(ConstraintNetworkBuilder cn) {
        this.cn = cn;
    }

    private void handleNode(Node n, Set<EquiClass> es) {

        if (cache.containsKey(n.getLabel())) {
            es.add(cache.getValueByKey(n.getLabel()));
            return;
        }

        if (n.isOperand()) {
            EquiClass eq = new EquiClass(new SingletonElement(n,n.getLabel
                    ()));
            LOGGER.debug("create equiclass {}:{}", eq.getDotLabel(), eq.getId());
            cache.put(n.getLabel(), eq);
            es.add(eq);
        } else {
            assert n.isOperation();
            createNestedElement(n, es);
        }
    }


    private void createNestedElement(Node n, Set<EquiClass> es) {
        LOGGER.debug("create nested element {} params {}", n, cn
                .getParametersFor(n).size());
        assert n.isOperation();

        List<Element> elems = new Vector<>();
        for (Node p : cn.getParametersFor(n)) {

            LOGGER.debug("handle node {}", p);
            handleNode(p, es);
            Set<Element> ess = cache.getValueByKey(p.getLabel()).getElements();
            LOGGER.debug("ESS {}", ess.toString());
            LOGGER.debug("SIZ " + ess.size());
            assert !ess.isEmpty();
            //Element e = ess.iterator().next();
            //assert e != null;

            // it is always enought to add only one single element
            elems.add(ess.iterator().next());
        }

        LOGGER.debug("elements");

        assert !n.getKind().toString().isEmpty();

        NestedElement<Node> nested = new NestedElement(n, n.getLabel(), n
                .getKind()
                .toString(),
                elems.toArray(new Element[elems.size()]));

        nested.setAnnotation(n.getKind().getDesc());

        // the equiclass that represents this node
        EquiClass nst = new EquiClass(nested);

        LOGGER.debug("create new nested element {}:{}", nst.getDotLabel(), n
                .getId());
        // check if there is already another equiclass for this particular
        // nested element
        //EquiClass eq = euf.inferEquiClassFor(nst);
        //eq = eq.union(nst);

        //cache.put(n, eq);

        cache.put(n.getLabel(), nst);
        es.add(nst);
    }


    @Override
    public Collection<EquiClass> createEquiClasses(Node... nods) {

        LOGGER.debug("create equi classes for:");
        for (Node n : nods) {
            LOGGER.debug("... {}", n.getLabel());
        }
        LOGGER.debug("done");

        Set<EquiClass> s = new HashSet<>();

        Set<EquiClass> ret = new HashSet<>();

        List<Element> ele = new Vector<>();

        for (int k = 0; k < nods.length; k++) {

            Node nod = nods[k];

            handleNode(nod, s);

            assert cache.containsKey(nod.getLabel());

            Collection<Element> cele = cache.getValueByKey(nod.getLabel())
                    .getElements();

            //LOGGER.debug("elements {}",cele.toString());
            //assert cele.size() == 1;

            //Element nodele = cele.iterator().next();

            //LOGGER.debug("nele {}", nodele);

            ele.addAll(cele);
        }

        EquiClass top = new EquiClass(ele);
        LOGGER.debug("additional equi class {}", top.getDotLabel());

        ret.add(top);
        ret.addAll(top.infer());
        ret.addAll(s);

        // infer additional facts
        Set<EquiClass> addfacts = s.stream().map(v -> v.infer())
                .flatMap(x -> x.stream()).collect(Collectors.toSet());

        ret.addAll(addfacts);

        return ret;
    }

    @Override
    public Collection<EquiClass> createEquiClass(Node p) {
        return createEquiClasses(p);
    }


    public String computeLabel(Element... s) {

        StringBuffer sb = new StringBuffer();

        if (s.length == 1) {
            sb.append(s[0].getLabel());
        } else {
            sb.append(s[0].getAnnotation());
            sb.append("(");
            for (int i = 1; i < s.length; i++) {
                sb.append(s[i].getLabel());
                if (i < s.length - 1)
                    sb.append(",");
            }
            sb.append(")");
        }
        return sb.toString();
    }

    @Override
    public EquiClass[] getEquiClassesFor(Node... ns) throws MissingItemException {


        LOGGER.debug("create equi classes");

        EquiClass[] ec = new EquiClass[ns.length];

        for (int k = 0; k < ns.length; k++) {
            Node n = ns[k];
            ec[k] = getEquiClassFor(n);
        }

        return ec;
    }

    @Override
    public EquiClass getEquiClassFor(Node n) throws MissingItemException {

        if (!cache.containsKey(n.getLabel()))
            throw new MissingItemException("Node " + n.getLabel() + " is " +
                    "not present");

        assert cache.containsKey(n.getLabel());

        return cache.getValueByKey(n.getLabel());
    }


    @Override
    public boolean hasEquiClassFor(Node n) {
        return this.cache.containsKey(n
                .getLabel());
    }

    @Override
    public void remove(Node n) {
        int id = n.getId();
        if(cn.containsVertex(n)) {
            cn.removeVertex(n);
        }

        assert cn.getConstraintNetwork().vertexSet().stream().filter(x -> x
                .getId() == id).count() == 0;
    }

    @Override
    public String toString() {
        return this.cache.toString();
    }




    public NodeCache getNodeCache() {
        return cache;
    }
}

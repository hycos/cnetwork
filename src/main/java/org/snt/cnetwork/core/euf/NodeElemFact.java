package org.snt.cnetwork.core.euf;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.core.Operand;
import org.snt.cnetwork.exception.MissingItemException;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Generate Equivalence Classes from Nodes
 */
public final class NodeElemFact implements EquiClassFact {

    final static Logger LOGGER = LoggerFactory.getLogger(NodeElemFact.class);

    private ConstraintNetworkBuilder cn;

    private NodeCache nc = new NodeCache();


    public String getLabelForNode(Node n) {
        assert nc.hasEquiClass(n);
        return nc.getLabel(n);
    }

    public Node getNodeForLabel(String lbl) {
        assert nc.hasNode(lbl);
        return nc.getNode(lbl);
    }

    public EquiClass getEquiClassForLabel(String lbl) {
        assert nc.hasNode(lbl);
        return nc.getEquiClass(lbl);
    }

    public NodeElemFact(ConstraintNetworkBuilder cn,
                        NodeElemFact ne) {
        this(cn);
        // clone cache
        nc = new NodeCache(ne.nc);
    }

    public NodeElemFact(ConstraintNetworkBuilder cn) {
        this.cn = cn;
    }

    private void handleNode(Node n, Set<EquiClass> es) {

        if (nc.hasEquiClass(n)) {
            es.add(nc.getEquiClass(n));
            return;
        }

        if (n.isOperand()) {
            EquiClass eq = new EquiClass(new SingletonElement(n,n.getLabel()));
            LOGGER.debug("create equiclass {}:{}", eq.getDotLabel(), eq.getId());

            nc.put(n, eq);

            es.add(eq);
        } else {
            assert n.isOperation();
            createNestedElement(n, es);
        }
    }


    public String getLabel(NodeKind kind, List<Node> params) {
        return kind.toString() + "(" + getParameterList(params) + ")";
    }

    private String getParameterList(List<Node> params) {

        String label = "";

        for (int i = 0; i < params.size(); i++) {
            Node par = params.get(i);
            assert (par != null);
            //LOGGER.info("Par " + par.getKind());
            //LOGGER.info("Par" + par.getId());
            //LOGGER.info("+SPLIT " + par.getLabel());
            String plbl = par.getLabel();
            if (par instanceof Operand && par.isLiteral() && par.isString()) {
                plbl = par.getLabel();
            }

            label += plbl + (i < params.size() - 1 ? "," : "");
        }

        return label;
    }

    private void createNestedElement(Node n, Set<EquiClass> es) {
        LOGGER.debug("create nested element {} params {}", n, cn
                .getParametersFor(n).size());
        assert n.isOperation();

        List<Element> elems = new Vector<>();
        for (Node p : cn.getParametersFor(n)) {

            LOGGER.debug("handle node {}", p);
            handleNode(p, es);
            Set<Element> ess = nc.getEquiClass(p).getElements();
            LOGGER.debug("ESS {}", ess.toString());
            LOGGER.debug("SIZ " + ess.size());
            assert !ess.isEmpty();
            //Element e = ess.iterator().next();
            //assert e != null;

            // it is always enought to add only one single element
            elems.add(ess.iterator().next());
        }

        LOGGER.debug("elements {}", elems);

        assert !n.getKind().toString().isEmpty();

        LOGGER.debug("compute label {}", computeParList(elems));

        String label = n.getKind().getDesc() + computeParList(elems);

        NestedElement nested = new NestedElement(n, label, n
                .getKind()
                .toString(),
                elems.toArray(new Element[elems.size()]));

        //nested.setAnnotation(n.getKind().getDesc());

        // the equiclass that represents this node
        EquiClass nst = new EquiClass(nested);

        LOGGER.debug("create new nested element {}:{}", nst.getDotLabel(), n
                .getId());
        // check if there is already another equiclass for this particular
        // nested element
        //EquiClass eq = euf.inferEquiClassFor(nst);
        //eq = eq.union(nst);

        //cache.put(n, eq);

        nc.put(n, nst);
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

            assert nc.hasEquiClass(nod);

            EquiClass e = nc.getEquiClass(nod);

            assert e != null;

            Collection<Element> cele = e .getElements();

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


    public String computeParList(Collection<Element> s) {
        return computeParList(s.toArray(new Element[s.size()]));
    }

    public String computeParList(Element... s) {

        StringBuffer sb = new StringBuffer();

        if (s.length == 1) {
            sb.append(s[0].getLabel());
        } else {
            //sb.append(s[0].getAnnotation());
            sb.append("(");
            for (int i = 0; i < s.length; i++) {
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

        if (!nc.hasEquiClass(n))
            throw new MissingItemException("Node " + n.getLabel() + " is " +
                    "not present");

        assert nc.hasEquiClass(n);

        return nc.getEquiClass(n);
    }


    @Override
    public boolean hasEquiClassFor(Node n) {
        return nc.hasEquiClass(n);
    }

    /**
     * Remove a node and transfer all outgoing connections to another node
     * @param toReplace
     * @param replacement
     */
    @Override
    public void relink(Node toReplace, Node replacement) {
        cn.relink(toReplace, replacement);
    }


}

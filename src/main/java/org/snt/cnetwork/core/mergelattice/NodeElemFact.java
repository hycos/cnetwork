package org.snt.cnetwork.core.mergelattice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.exception.MissingItemException;

import java.util.*;
import java.util.stream.Collectors;

public final class NodeElemFact implements EquiClassFact<Node> {

    final static Logger LOGGER = LoggerFactory.getLogger(NodeElemFact.class);

    private ConstraintNetwork cn;
    private Map<Integer, EquiClass> escache = new HashMap<>();

    public NodeElemFact(ConstraintNetwork cn, NodeElemFact ne) {
        this(cn);
        // clone cache
        for(Map.Entry<Integer, EquiClass> e : ne.escache.entrySet()) {
            escache.put(e.getKey(), e.getValue());
        }
    }

    public NodeElemFact(ConstraintNetwork cn) {
        this.cn = cn;
    }

    private void handleNode(Node n, Set<EquiClass> es) {

        if(escache.containsKey(n.getId())) {
            es.add(escache.get(n.getId()));
            return;
        }

        if (n.isOperand()) {
            EquiClass eq = new EquiClass(new SingletonElement(n.getLabel()));
            escache.put(n.getId(), eq);
            es.add(eq);
        } else {
            assert n.isOperation();
            createNestedElement(n, es);
        }
    }


    private void createNestedElement(Node n, Set<EquiClass> es) {
        LOGGER.debug("create nested element {} params {}",n, cn
                .getParametersFor(n).size());
        assert n.isOperation();

        List<Element> elems = new Vector<>();
        for (Node p : cn.getParametersFor(n)) {
            handleNode(p,es);
            Set<Element> ess = escache.get(p.getId()).getElements();
            assert ess.size() == 1;
            Element e = ess.iterator().next();
            assert e != null;
            elems.add(e);
        }

        LOGGER.debug("elements");
        NestedElement nested = new NestedElement(n.getLabel(),
                elems.toArray(new Element[elems.size()]));
        nested.setAnnotation(n.getKind().toString());

        EquiClass eq = new EquiClass(nested);

        escache.put(n.getId(), eq);
        es.add(eq);
    }


    @Override
    public Collection<EquiClass> createEquiClasses(Node... nods) {

        Set<EquiClass> s = new HashSet<>();

        Set<EquiClass> ret = new HashSet<>();

        List<Element> ele = new Vector<>();

        for(int k = 0; k < nods.length; k++) {

            Node nod = nods[k];

            handleNode(nod,s);

            assert escache.containsKey(nod.getId());

            Collection<Element> cele = escache.get(nod.getId()).getElements();
            assert cele.size() == 1;

            Element nodele = cele.iterator().next();

            LOGGER.debug("nele {}", nodele);

            ele.add(nodele);
        }

        EquiClass top = new EquiClass(ele);
        LOGGER.debug("additional equi class {}", top);

        ret.add(top);
        ret.addAll(top.infer());
        ret.addAll(s);

        // infer additional facts
        Set<EquiClass> addfacts = s.stream().map(v -> v.infer())
                .flatMap(x-> x.stream()).collect(Collectors.toSet());

        ret.addAll(addfacts);

        return ret;
    }

    @Override
    public Collection<EquiClass> createEquiClass(Node p) {
       return createEquiClasses(p);
    }

    @Override
    public String computeLabel(Element... s) {

        StringBuffer sb = new StringBuffer();

        if(s.length == 1) {
            sb.append(s[0].getLabel());
        } else {
            sb.append(s[0].getAnnotation());
            sb.append("(");
            for(int i = 1; i < s.length; i ++) {
                sb.append(s[i].getLabel());
                if(i < s.length-1)
                    sb.append(",");
            }
            sb.append(")");
        }
        return sb.toString();
    }

    @Override
    public EquiClass[] getEquiClassesFor(Node ... ns) throws MissingItemException {


        LOGGER.debug("create equi classes");

        EquiClass [] ec = new EquiClass[ns.length];

        for(int k = 0; k < ns.length; k++ ) {
            Node n = ns[k];
            ec[k] = getEquiClassFor(n);
        }

        return ec;
    }

    @Override
    public EquiClass getEquiClassFor(Node n) throws MissingItemException {

        if(!escache.containsKey(n.getId()))
            throw new MissingItemException("Node " + n.getLabel() + " is " +
                    "not present");

        assert escache.containsKey(n.getId());

        return escache.get(n.getId());
    }

    @Override
    public boolean hasEquiClassFor(Node n) {
        return this.escache.containsKey(n);
    }

    /**@Override
    public boolean areEquivalent(Node a, Node b) {

        assert escache.containsKey(a.getId());
        assert escache.containsKey(b.getId());

        return true;
    }**/


    public String computeLabel(String... s) {

        StringBuffer sb = new StringBuffer();

        if(s.length == 1) {
            sb.append(s[0]);
        } else {
            sb.append(s[0]);
            sb.append("(");
            for(int i = 1; i < s.length; i ++) {
                sb.append(s[i]);
                if(i < s.length-1)
                    sb.append(",");
            }
            sb.append(")");
        }
        return sb.toString();
    }



}

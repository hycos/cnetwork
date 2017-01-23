package org.snt.cnetwork.core.mergelattice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;

import java.util.*;

public class NodeElemFact implements EquiClassFact<Node> {

    final static Logger LOGGER = LoggerFactory.getLogger(NodeElemFact.class);


    private ConstraintNetwork cn;

    public NodeElemFact(ConstraintNetwork cn) {
        this.cn = cn;
    }

    private void handleNode(Node n, Map<Node,EquiClass> es) {

        if (n.isOperand()) {
            es.put(n,new EquiClass(new SingletonElement(n.getLabel())));
        } else {
            assert n.isOperation();
            createNestedElement(n, es);
        }
    }


    private void createNestedElement(Node n, Map<Node,EquiClass> es) {
        LOGGER.debug("create nested element {} params {}",n, cn
                .getParametersFor(n).size());
        assert n.isOperation();

        List<Element> elems = new Vector<>();
        for (Node p : cn.getParametersFor(n)) {
            handleNode(p,es);
            Set<Element> ess = es.get(p).getElements();
            assert ess.size() == 1;
            Element e = ess.iterator().next();
            assert e != null;
            elems.add(e);
        }

        LOGGER.debug("elements");
        NestedElement nested = new NestedElement(n.getLabel(),elems.toArray
                (new Element[elems.size()]));
        nested.setAnnotation(n.getKind().toString());

        es.put(n,new EquiClass(nested));
    }


    @Override
    public Collection<EquiClass> createEquiClasses(Node... nods) {

        Map<Node,EquiClass> s = new HashMap<>();

        Set<EquiClass> ret = new HashSet<>();

        List<Element> ele = new Vector<Element>();

        for(int k = 0; k < nods.length; k++) {

            Node nod = nods[k];

            handleNode(nod,s);

            assert s.containsKey(nod);

            Collection<Element> cele = s.get(nod).getElements();
            assert cele.size() == 1;

            Element nodele = cele.iterator().next();

            LOGGER.debug("nele {}", nodele);

            ele.add(nodele);
        }

        EquiClass top = new EquiClass(ele);
        LOGGER.debug("additional equi class {}", top);

        ret.add(top);
        ret.addAll(s.values());


        return ret;
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

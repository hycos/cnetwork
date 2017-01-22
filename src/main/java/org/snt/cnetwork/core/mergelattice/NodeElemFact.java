package org.snt.cnetwork.core.mergelattice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.Node;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

public class NodeElemFact implements EquiClassFact<Node> {

    final static Logger LOGGER = LoggerFactory.getLogger(NodeElemFact.class);

    @Override
    public EquiClass create(Node []... pars) {
        LOGGER.debug("he");

        EquiClass eq = new EquiClass();
        for(int i = 0; i < pars.length; i++) {
            eq.addAll(create(pars[i]));

        }
        return eq;
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


    private Set<Element> create(Node [] nods) {

        assert nods.length > 0;

        Node p0 = nods[0];

        Set<Element> eset = new LinkedHashSet<>();

        if(p0.isOperand()) {
            String [] spars = new String [nods.length];
            for(int i = 0; i < nods.length; i ++ ) {
                spars[i] = nods[i].getLabel();
                eset.add(new ElementSingleton(nods[i].getLabel()));
            }
        } else {
            assert p0.isOperation();

            LOGGER.debug("N {}", nods.length);

            String [] pp = new String [nods.length];

            for(int i = 0; i < nods.length; i ++ ) {
                LOGGER.debug("I {}", i);
                if(i == 0) {
                    pp[i] = nods[i].getKind().toString();
                } else {
                    pp[i] = nods[i].getLabel();
                }
            }

            String lbl = computeLabel(pp);

            LOGGER.debug("lbl {}", lbl);

            ElementTuple et = new ElementTuple(lbl, Arrays.copyOfRange(pp,1,
                    pp.length));
            et.setAnnotation(p0.getKind().toString());
            eset.add(et);
        }

        return eset;
    }


}

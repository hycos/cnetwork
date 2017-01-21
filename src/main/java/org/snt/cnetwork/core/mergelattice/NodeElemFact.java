package org.snt.cnetwork.core.mergelattice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.Node;

public class NodeElemFact implements EquiClassFact<Node> {

    final static Logger LOGGER = LoggerFactory.getLogger(NodeElemFact.class);

    @Override
    public EquiClass [] create(Node []... pars) {
        LOGGER.debug("he");
        EquiClass [] ret = new EquiClass[pars.length];
        for(int i = 0; i < pars.length; i++) {
            ret[i] = new EquiClass(create(pars[i]));

        }
        return ret;
    }


    private Element create(Node [] pars) {
        if(pars.length == 1) {
            return new ElementSingleton(pars[0].getLabel());
        } else {

            String [] spars = new String [pars.length];
            for(int i = 0; i < pars.length; i ++ ) {
                spars[i] = pars[i].getLabel();
            }
            return new ElementTuple(computeLabel(pars),spars);
        }

    }

    @Override
    public String computeLabel(Node... pars) {

        StringBuffer sb = new StringBuffer();

        for(int i = 0; i < pars.length; i ++) {
            if(i == 0) {
                sb.append(pars[i].getKind().toString());
                sb.append("(");
                continue;
            }

            sb.append(pars[i].getLabel());

            if(i < pars.length-1)
                sb.append(",");
        }

        sb.append(")");
        return sb.toString();
    }

}

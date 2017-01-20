package org.snt.cnetwork.tools.mtree;


import org.snt.cnetwork.core.Node;

import java.util.*;

public class NodeBag extends Element {

    private List<Node> bag = new Vector<>();

    public NodeBag(Node ...n) {
        bag.addAll(Arrays.asList(n));
    }

    public NodeBag(Node op, Collection<Node> params) {
        bag.add(op);
        bag.addAll(params);
    }

    public boolean isOperation() {
        if (bag.size() > 1) {
            Node n = bag.iterator().next();
            assert n.isOperation();
            return true;
        }
        return false;
    }

    public boolean isOperand() {
        if (bag.size() == 1) {
            Node n = bag.iterator().next();
            assert n.isOperand();
            return true;
        }
        return false;
    }

    @Override
    public String getLabel() {

        if(isOperand()){
            assert bag.size() == 1;
            return bag.iterator().next().getLabel();
        } else {
            assert (isOperation());
            StringBuffer sb = new StringBuffer();
            for(Node n : bag) {
                if(n.isOperation()) {
                    sb.append(n.getKind().getDesc() + "(");
                } else {
                    if(sb.charAt(sb.length()-1) != '('){
                        sb.append(",");
                    }
                    sb.append(n.getLabel());
                }
            }
            sb.append(")");
            return sb.toString();
        }
    }

    @Override
    public Collection<Element> split() {
        LinkedHashSet<Element> ret = new LinkedHashSet<>();
        if(isOperand()) {
            ret.add(this);
        } else {
            for(int i = 1; i < bag.size(); i++) {
                ret.add(new NodeBag(bag.get(i)));
            }
        }
        return ret;
    }

    @Override
    public boolean isSplittable() {
        return this.isOperation();
    }


}

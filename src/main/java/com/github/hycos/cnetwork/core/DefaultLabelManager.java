/*
 * polyglot - translate constraints in between different formats
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * polyglot is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetwork.core;

import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkInterface;
import com.github.hycos.cnetwork.api.labelmgr.LabelManagerInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.utils.BiMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DefaultLabelManager implements LabelManagerInterface<Node>  {

    final static Logger LOGGER = LoggerFactory.getLogger(DefaultLabelManager.class);

    BiMap<Node, String> lblmap  = new BiMap<>();
    ConstraintNetworkInterface<Node> c = null;


    public DefaultLabelManager() {

    }

    public DefaultLabelManager(DefaultLabelManager other) {
        lblmap.putAll(other.lblmap);
    }

    @Override
    public String computeLabel(Node n) {

        if(lblmap.containsKey(n)) {
            return lblmap.getValueByKey(n);
        }

        if(n.isOperand()) {
            LOGGER.debug("kind {}", n.getKind());
            LOGGER.debug("literal {}", n.getKind().isLiteral());
            if(n.getKind().isLiteral() && n.getKind().isString()) {
                LOGGER.debug("ccc {}","Operand");
                return "\"" + n.getShortLabel() + "\"";
            }
            return n.getShortLabel();
        } else {
            String parlist = "";
            for(Node par : c.getParametersFor(n) ) {
                if(!parlist.isEmpty())
                    parlist += ",";

                parlist += computeLabel(par);
            }

            String pfx = "";

            if(n.getKind() == DefaultNodeKind.EXTERNAL)
                pfx = n.getSignature().toBCString();
            else
                pfx = n.getKind().toString();

            return pfx + "(" + parlist + ")";
        }

    }

    @Override
    public Node getNodeByLabel(String lbl) {
        return lblmap.getKeyByValue(lbl);
    }

    @Override
    public boolean hasNodeForLabel(String lbl) {
        return lblmap.containsValue(lbl);
    }

    @Override
    public String getLabelForNode(Node n) {
        return lblmap.getValueByKey(n);
    }

    @Override
    public void setLabelForNode(Node n, String lbl) {
        lblmap.put(n, lbl);
    }

    @Override
    public Node infer(Node n) throws InconsistencyException {
        Node r = lblmap.getKeyByValue(computeLabel(n));
        if(r != null)
            return r;

        return n;
    }

    @Override
    public LabelManagerInterface<Node> clone() {
        return new DefaultLabelManager(this);
    }

    @Override
    public void onNodeCollapse(Node toReplace, Node replacement) throws InconsistencyException {

    }

    @Override
    public void onNodeDelete(Node deleted) {

    }

    @Override
    public void onNodeAdd(Node n) {
        n.setLabelManager(this);
        lblmap.put(n, computeLabel(n));
    }

    @Override
    public void onConstraintAdd(Node operand) throws InconsistencyException {

    }

    @Override
    public void onConnectionAdd(Node frst, Node snd) {

    }

    @Override
    public void update(Node n) throws InconsistencyException {

    }

    @Override
    public void attach(Node n) {
        n.attach(this);
    }

    @Override
    public void register(ConstraintNetworkInterface<Node> c) {
        this.c = c;
    }
}

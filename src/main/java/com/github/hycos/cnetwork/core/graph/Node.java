/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
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

package com.github.hycos.cnetwork.core.graph;

import com.github.hycos.cnetwork.api.NodeInterface;
import com.github.hycos.cnetwork.api.NodeKindInterface;
import com.github.hycos.cnetwork.api.domctrl.Domain;
import com.github.hycos.cnetwork.api.domctrl.DomainControllerInterface;
import com.github.hycos.cnetwork.api.domctrl.Term;
import com.github.hycos.cnetwork.api.domctrl.exception.DomainControllerException;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkSubject;
import com.github.hycos.cnetwork.api.labelmgr.LabelManagerInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.sig.JavaMethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

public abstract class Node extends ConstraintNetworkSubject<Node> implements
        NodeInterface,Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(Node.class);

    protected final int id;

    protected String instance = "";
    protected String shortLabel = "";
    protected String annotation = "";
    protected String note = "";

    public String getNote() {
        return note;
    }

    public void setNote(String note) {
        this.note = note;
    }

    private DomainControllerInterface<Node> dctrl = null;
    private LabelManagerInterface<Node> lmgr = null;

    private static int nid = 0;
    private NodeKindInterface kind = null;


    public Node(String shortLabel, NodeKindInterface kind) {
        this.id = nid++;
        this.kind = kind;
        this.shortLabel = shortLabel;
    }

    public Node(Node other) {
        this.id = other.id;
        // just the reference
        this.dctrl = other.dctrl;
        this.annotation = other.annotation;
        this.kind = other.kind;
        this.shortLabel = other.shortLabel;
    }

    public int getId() {
        return this.id;
    }


    public boolean hasDomainController() {
        return this.dctrl != null;
    }

    public boolean hasLabelManager() {
        return this.lmgr != null;
    }

    // Eveery single node can be uniqely identified
    @Override
    public int hashCode() {
        return this.id;
    }

    @Override
    public boolean equals(Object o) {

        if (!(o instanceof Node))
            return false;

        Node n = (Node) o;

        return this.id == n.id;
    }

    public Domain getDomain() {
        Objects.nonNull(this.dctrl);
        assert this.dctrl.hasDomain(this);
        try {
            return this.dctrl.getDomain(this);
        } catch (DomainControllerException e) {
            assert false;
        }
        return null;
    }

    public void setDomain(Domain d) throws InconsistencyException {
        Objects.nonNull(this.dctrl);
        assert this.dctrl != null;
        try {
            this.dctrl.setDomain(this, d);
        } catch (DomainControllerException e) {
            throw new InconsistencyException(e.getMessage());
        }
        onDomainChange(this);
    }

    public NodeKindInterface getKind() {
        return this.kind;
    }

    public void setKind(NodeKindInterface kind) {
        this.kind = kind;
    }

    public abstract boolean isOperation();

    public abstract boolean isOperand();

    public boolean isLiteral() {
        return getDomain().isLiteral();
    }

    public boolean isRegex() {
        return getDomain().isRegex();
    }

    public boolean isString() {
        return getDomain().isString();
    }

    public boolean isNumeric() {
        return getDomain().isNumeric();
    }

    public boolean isBoolean() {
        return getDomain().isBoolean();
    }

    public boolean isVariable() {
        return getDomain().isVariable();
    }

    public boolean isConstraint() {
        return getDomain().isConstraint();
    }

    public boolean isEquality() {
        return getDomain().isConstraint();
    }

    public boolean isInequality() {
        return getDomain().isConstraint();
    }

    public void setInstance(String instance) {
        this.instance = instance;
    }

    public abstract void setSignature(JavaMethodSignature signature);
    public abstract JavaMethodSignature getSignature();


    public String getDotLabel() {
        final StringBuilder s = new StringBuilder();
        s.append("{" + this.id + "}\\n");
        s.append(isAnnotated() ? getAnnotation() + "\\n" : "");
        s.append("dom:" + getDomain().getLabel() +"\\n");
        s.append("kind:" + getKind().getDesc() +"\\n");
        if(!note.isEmpty())
            s.append("note:" + note + "\\n");
        return s.toString();
    }

    public String getLabel() {
        Objects.requireNonNull(this.lmgr);
        return this.lmgr.getLabelForNode(this);
    }

    public String getShortLabel() {
        return this.shortLabel;
    }

    @Override
    public String toString() {
        return "" + id;
    }

    public void setLabel(String label) {
        this.lmgr.setLabelForNode(this, label);
    }

    public void annotate(String annotation) {
        this.annotation = annotation;
    }

    public boolean isAnnotated() {
        return !this.annotation.isEmpty();
    }

    public String getAnnotation() {
        return this.annotation;
    }

    public void setDomainController(DomainControllerInterface dctrl) {
        this.dctrl = dctrl;
    }

    public void setLabelManager(LabelManagerInterface<Node> lmgr) {
        this.lmgr = lmgr;
    }

    @Override
    public abstract Node clone();


    @Override
    public void setTerm(Term t){

    }

    @Override
    public Term getTerm(){
        return null;
    }

}

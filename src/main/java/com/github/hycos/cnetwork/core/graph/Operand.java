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


import com.github.hycos.cnetwork.api.NodeKindInterface;
import com.github.hycos.cnetwork.utils.EscapeUtils;

public class Operand extends Node {

    public Operand(Operand o) {
        super(o);
    }

    public Operand(ConstraintNetwork cn, String label, String kind) {
       this(cn ,label, DefaultNodeKind.KindFromString(kind));
    }

    public Operand(ConstraintNetwork cn, String label, NodeKindInterface
            kind) {
        super(cn,label,kind);
    }

    @Override
    public boolean isOperation() {
        return false;
    }

    @Override
    public boolean isOperand() {
        return true;
    }

    @Override
    public boolean isConstraint() {
        return false;
    }


    public String getName() {
        return this.shortLabel;
    }

    @Override
    public Operand clone() { return new Operand(this); }

    @Override
    public String getDotLabel() {
        return EscapeUtils.escapeSpecialCharacters(shortLabel) + "\\nn" + super.getDotLabel();
    }

}

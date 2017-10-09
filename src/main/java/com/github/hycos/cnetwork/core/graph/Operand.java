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


import com.github.hycos.cnetwork.utils.EscapeUtils;

public class Operand extends Node {

    public Operand(Operand o) {
        super(o);
    }

    public Operand(String label, String kind) {
       this(label, NodeKind.KindFromString(kind));
    }

    public Operand(String label, NodeKind kind) {
        super(label,kind);
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
    public boolean isLiteral() {
        return this.kind.isLiteral();
    }

    @Override
    public boolean isRegex() {
        return this.kind.isRegex();
    }

    @Override
    public boolean isString() {
        return this.kind.isString();
    }

    @Override
    public boolean isNumeric() {
        return this.kind.isNumeric();
    }

    @Override
    public boolean isBoolean() {
        return this.kind.isBoolean();
    }

    @Override
    public boolean isVariable() {
        return !this.kind.isLiteral() && !this.kind.isRegex();
    }

    @Override
    public boolean isConstraint() {
        return false;
    }


    public String getName() {
        return this.label;
    }

    @Override
    public Operand clone() { return new Operand(this); }

    @Override
    public String getDotLabel() {
        return EscapeUtils.escapeSpecialCharacters(label) + "\\nn" + super
                .getDotLabel();
    }

}

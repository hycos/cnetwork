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

package com.github.hycos.cnetwork.core.domain.automaton.trackauto;

import com.github.hycos.cnetwork.core.domain.range.NumRange;
import dk.brics.automaton.Automaton;

public class TrackAutomatonNode implements Comparable<TrackAutomatonNode> {

    public enum Kind {
        UNION,
        INTERSECTION,
        MINUS,
        CONCAT,
        PLUS,
        STAR,
        COMPLEMENT,
        LEAF
    }

    private Automaton a = null;
    private NumRange ran = null;
    private String name = "";
    private Kind kind = Kind.LEAF;
    private int id = 0;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public TrackAutomatonNode(Kind kind,
                              Automaton a,
                              NumRange nr,
                              int id,
                              String name) {
        this.a = a;
        this.ran = nr;
        this.kind = kind;
        this.id = id;
        this.name = name;
    }

    public TrackAutomatonNode(TrackAutomatonNode n) {
        this.a = n.a.clone();
        this.ran = n.getRange().clone();
        this.kind = n.kind;
        this.id = n.id;
        this.name = n.name;
    }

    public Automaton getAutomaton() {
        return a;
    }

    public NumRange getRange() {
        return ran;
    }

    public void setAutomaton(Automaton a) {
        this.a = a;
    }

    public void setRange(NumRange nr) {
        this.ran = ran;
    }

    public Kind getKind() {
        return kind;
    }

    public void setKind(Kind k) {
        this.kind = k;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public int hashCode() {
        return this.id;
    }

    @Override
    public boolean equals(Object other){
        if(!(other instanceof TrackAutomatonNode))
            return false;

        TrackAutomatonNode o = (TrackAutomatonNode)other;

        return id == o.id;
    }

    @Override
    public int compareTo(TrackAutomatonNode n) {
        return id - n.getId();
    }
}

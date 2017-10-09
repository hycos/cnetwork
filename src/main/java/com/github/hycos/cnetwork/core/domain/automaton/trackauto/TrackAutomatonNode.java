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

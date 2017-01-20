package org.snt.cnetwork.tools.mtree;

import org.jgrapht.graph.DefaultEdge;

public class EquiEdge<T extends Element> extends DefaultEdge implements
        Cloneable {

    private EquiClass<T> src;
    private EquiClass<T> dst;
    private Kind kind;
    private int sequence = -1;

    public EquiEdge() {
        kind = Kind.EQUI;
    }

    public EquiEdge(EquiClass<T> src, EquiClass<T> dst) {
        this(src,dst,Kind.EQUI, -1);
    }

    public EquiEdge(EquiClass<T> src, EquiClass<T> dst, Kind kind,
                    int sequence) {
        this.dst = dst;
        this.src = src;
        this.kind = kind;
        this.sequence = sequence;
    }

    public enum Kind {
        EQUI(0,"equi"),
        PAR(1,"par"),
        SUB(2, "sub");

        private final String value;
        private int ival;

        Kind(int ival, String val) {
            this.value = val;
            this.ival = ival;
        }
        public static Kind KindFromString(String kind) {

            switch (kind) {
                case "par":
                    return PAR;
                case "equi":
                    return EQUI;
                case "sub":
                    return SUB;
            }
            // should never ever happen
            assert (false);
            return null;
        }
        public String getValue() {
            return this.value;
        }
        public int getId() {
            return ival;
        }
    }

    @Override
    public EquiClass<T> getSource() {
        return src;
    }

    @Override
    public EquiClass<T> getTarget() {
        return dst;
    }

    public Kind getKind(){
        return kind;
    }

    public int getSequence() {
        return this.sequence;
    }
}

package org.snt.cnetwork.core.mergelattice;

import org.jgrapht.graph.DefaultEdge;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EquiEdge extends DefaultEdge implements
        Cloneable {

    final static Logger LOGGER = LoggerFactory.getLogger(EquiEdge.class);

    private EquiClass src;
    private EquiClass dst;
    private Kind kind;
    private int sequence = -1;

    public EquiEdge() {
        kind = Kind.EQUI;
    }

    public EquiEdge(EquiClass src, EquiClass dst) {
        this(src,dst,Kind.EQUI, -1);
    }

    public EquiEdge(EquiClass src, EquiClass dst, Kind kind,
                    int sequence) {

        LOGGER.debug("trying to add {} {}", src, dst);
        assert !src.equals(dst);
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
    public EquiClass getSource() {
        return src;
    }

    @Override
    public EquiClass getTarget() {
        return dst;
    }

    public Kind getKind(){
        return kind;
    }

    public int getSequence() {
        return this.sequence;
    }

    @Override
    public String toString() {
        return src.toString() + "->" + dst.toString();
    }
}

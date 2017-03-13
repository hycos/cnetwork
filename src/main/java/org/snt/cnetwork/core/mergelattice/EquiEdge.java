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


    public EquiEdge(EquiClass src, EquiClass dst, Kind kind,
                    int sequence) {

        LOGGER.debug("trying to add {} {}", src.getDotLabel(), dst.getDotLabel());
        assert !src.equals(dst);
        this.dst = dst;
        this.src = src;
        this.kind = kind;
        this.sequence = sequence;
    }

    public enum Kind {
        SPLIT(0,"split"),
        SUB(1, "sub"),
        INEQ(2, "ineq");

        private final String value;
        private int ival;

        Kind(int ival, String val) {
            this.value = val;
            this.ival = ival;
        }
        public static Kind KindFromString(String kind) {

            switch (kind) {
                case "par":
                    return SPLIT;
                case "sub":
                    return SUB;
                case "ineq":
                    return INEQ;
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
    public boolean equals(Object o) {
        if(!(o instanceof EquiEdge))
            return false;

        EquiEdge ee = (EquiEdge)o;

        return src.equals(ee.src) && dst.equals(ee.dst) && sequence == ee
                .sequence;
    }

    @Override
    public String toString() {
        return src.toString() + "-(" + sequence + ")->" + dst
                .toString();
    }

    @Override
    public int hashCode() {
        return toString().hashCode();
    }
}

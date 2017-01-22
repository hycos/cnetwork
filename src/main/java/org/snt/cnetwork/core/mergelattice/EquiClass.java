package org.snt.cnetwork.core.mergelattice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.utils.EscapeUtils;

import java.util.*;

public class EquiClass {

    final static Logger LOGGER = LoggerFactory.getLogger(EquiClass.class);

    protected Set<Element> set = new TreeSet<>();

    public static class Top extends EquiClass {
        @Override
        public String toString() {
            return "\u22A4";
        }

        @Override
        public String getDotLabel() {
            return toString();
        }

        @Override
        public boolean subsumes(EquiClass other) {
            return true;
        }

        @Override
        public boolean hasOverlap(EquiClass other) {
            return true;
        }

        @Override
        public int getCardinality() {
            return 1;
        }
    }

    public static class Bottom extends EquiClass {
        @Override
        public String toString() {
            return "\u22A5";
        }

        @Override
        public String getDotLabel() {
            return toString();
        }

        @Override
        public boolean subsumes(EquiClass other) {
            return false;
        }
        @Override
        public boolean hasOverlap(EquiClass other) {
            return false;
        }

        @Override
        public int getCardinality() {
            return 1;
        }
    }

    private static int nid = 0;
    private int id = 0;

    public EquiClass() {
        this.id = nid++;
    }

    public EquiClass(EquiClass other) {
        this(other.getElements());
    }

    public EquiClass(Element nod) {
        this();
        assert nod != null;
        assert set != null;
        set.add(nod);
    }

    public EquiClass(Element [] nods) {
        this(Arrays.asList(nods));
    }

    public EquiClass(Collection<Element> nods) {
        this();
        set.addAll(nods);
    }

    public void add(Element ele) {
        set.add(ele);
    }
    public void addAll(Collection<Element> ele) {
        set.addAll(ele);
    }

    public String getDotLabel() {
        StringBuffer sb = new StringBuffer();
        sb.append("\\{");
        for(Element n : set) {
            if(sb.charAt(sb.length()-1) != '{') {
                sb.append(",");
            }
            sb.append(EscapeUtils.escapeSpecialCharacters(n.getLabel()));
        }
        sb.append("\\}");
        return sb.toString();
    }

    protected Map<Element, EquiClass> convertElementoMap() {
        Map<Element, EquiClass> mp = new HashMap<>();
        for(Element n : set) {
            mp.put(n, this);
        }
        return mp;
    }

    public Set<Element> getElements() {
        return set;
    }

    public boolean isSingleton() {
        return set.size() == 1;
    }

    public void addElements(Set<Element> nodes) {
        set.addAll(nodes);
    }

    public int getId() {
        return this.id;
    }

    public boolean isEmpty() {
        return this.set.isEmpty();
    }

    @Override
    public int hashCode() {
        return getDotLabel().hashCode();
    }

    @Override
    public boolean equals(Object o) {
        if(!(o instanceof EquiClass))
            return false;

        EquiClass ec = (EquiClass)o;

        return this.hashCode() == ec.hashCode();
    }

    @Override
    public String toString() {
        return getDotLabel();
    }

    public boolean hasOverlap(EquiClass other) {
        return !intersection(other).isEmpty();
    }

    public EquiClass intersection(EquiClass other) {
        Set<Element> isect = new HashSet<>(set);
        isect.retainAll(other.set);
        return new EquiClass(isect);
    }


    public boolean subsumes(EquiClass other) {
        return intersection(other).equals(other);
    }

    public boolean isAtomic() {
        return isSingleton() && !set.iterator().next().isTuple();
    }

    public boolean isSingletonTuple() {
        return isSingleton() && set.iterator().next().isTuple();
    }

    public int getCardinality() {
        return this.set.size();
    }


    private Collection<EquiClass> singletonSplit() {

        assert isSingleton();

        Set<EquiClass>ret = new LinkedHashSet<>();

        for(Element e : set) {
            LOGGER.debug("split {}", e);
            for(Element split : e.split()) {
                assert split != null;
                ret.add(new EquiClass(split));
            }
        }

        return ret;

    }

    private Collection<EquiClass> multiSplit() {

        assert !isSingleton();

        Set<EquiClass>ret = new LinkedHashSet<>();

        for(Element e : set) {
            ret.add(new EquiClass(e));
        }
        return ret;
    }


    public Collection<EquiClass> split() {

        if(isSingleton())
            return singletonSplit();

        return multiSplit();
    }

}
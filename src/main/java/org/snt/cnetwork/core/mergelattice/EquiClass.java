package org.snt.cnetwork.core.mergelattice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.utils.EscapeUtils;

import java.util.*;
import java.util.stream.Collectors;

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

        @Override
        public boolean isAtomic() {
            return true;
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

        public boolean isAtomic() {
            return true;
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

    public boolean isNested() {
        return this.set.size() == 1 && set.iterator().next() instanceof
                NestedElement;
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

    public EquiClass union(EquiClass other) {
        Set<Element> union = new HashSet<>(set);
        union.addAll(other.set);
        return new EquiClass(union);
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

        List<EquiClass>ret = new Vector<>();

        for(Element e : set) {
            LOGGER.debug("split {} {}", e, e.getClass().toGenericString());
            for(Element split : e.split()) {
                assert split != null;
                ret.add(new EquiClass(split));
            }
        }

        return ret;

    }

    private Collection<EquiClass> multiSplit() {

        assert !isSingleton();

        List<EquiClass>ret = new Vector<>();

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

    /**
     * Infer additional facts from the given ones
     * @return
     */
    public Set<EquiClass> infer() {

        LOGGER.debug("Infer additional facts for {} with set size {}", this,
                set.size());

        Set<EquiClass> ret = new HashSet<>();

        Map<String,List<Element>> m = new HashMap<>();


        for(Element e : set) {
            if(e.getAnnotation() != null) {
                String anno = e.getAnnotation();
                if(!m.containsKey(anno)) {
                    m.put(e.getAnnotation(), new Vector<>());
                }
                m.get(e.getAnnotation()).add(e);
            }
        }

        Set<List<Element>> tolink = m.values().stream().filter(v -> v.size()
                > 1)
                .collect(Collectors.toSet());

        if(tolink.isEmpty())
            return ret;

        List<Element> feset = tolink.iterator().next();

        int rows = feset.size();
        int cols = feset.iterator().next().split().length;

        LOGGER.debug("rows {}, cols {}", rows, cols);

        Element [][] elink = new Element[rows][rows];

        for(int row = 0; row < rows; row++){
            elink[row] = feset.get(row).split();
        }

        for(int col = 0; col < cols; col++){
            EquiClass ec = new EquiClass();
            for(int row = 0; row < rows; row++) {
                ec.add(elink[row][col]);
            }
            ret.add(ec);
            LOGGER.debug("new fact {}", ec);
        }

        return ret;
    }

}
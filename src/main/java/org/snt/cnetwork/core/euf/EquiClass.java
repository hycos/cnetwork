package org.snt.cnetwork.core.euf;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.utils.EscapeUtils;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class EquiClass implements Cloneable {


    final static Logger LOGGER = LoggerFactory.getLogger(EquiClass.class);

    public static final class Top extends EquiClass {
        @Override
        public String toString() {
            return "\u22A4";
        }

        @Override
        public String getDotLabel() {
            return toString();
        }

        @Override
        public String getLabel() { return toString(); };

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

        @Override
        public EquiClass clone() {
            return new Top();
        }

        @Override
        public EquiClass intersection(EquiClass c) {
            return new Top();
        }
    }

    public static final class Bottom extends EquiClass {
        @Override
        public String toString() {
            return "\u22A5";
        }

        @Override
        public String getDotLabel() {
            return toString();
        }

        @Override
        public String getLabel() { return toString(); };

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

        @Override
        public EquiClass clone() {
            return new Bottom();
        }

        @Override
        public EquiClass intersection(EquiClass c) {
            return new Bottom();
        }
    }

    private static int nid = 0;
    private final int id;

    // elements are ordered in alphabetical order
    protected final Set<Element> set = new HashSet<>();

    public EquiClass(Element ... nods) {
        this.id = nid++;
        set.addAll(Arrays.asList(nods));

    }

    public EquiClass(EquiClass other) {
        set.addAll(other.set);
        this.id = other.id;
    }


    public EquiClass(Collection<Element> nods) {
        this(nods.toArray(new Element [nods.size()]));
    }

    private void add(Element ele) {
        set.add(ele);
    }

    private void addAll(Collection<Element> ele) {
        set.addAll(ele);
    }

    public Set<Element> getElement(Predicate<Element> p) {

        try {
            return this.set.stream().filter(p).collect(Collectors.toSet());
        } catch (NoSuchElementException e) {
            return new HashSet<>();
        }
    }

    public Element getFirstElement(Predicate <Element> p) {
        try {
            return this.set.stream().filter(p).findFirst().get();
        } catch (NoSuchElementException e) {
            return null;
        }
    }

    public Element getFirstElement() {
        assert set.size() > 0;
        return set.iterator().next();
    }

    public String getDotLabel() {
        StringBuffer sb = new StringBuffer();
        for(Element n : set) {
            if(sb.length() > 0) {
                sb.append("\\n");
            }
            sb.append(EscapeUtils.escapeSpecialCharacters(n.getLabel()));
            sb.append("[" + n.mappedNode.toString() + "]");
            sb.append("(" + getId() + ")");
        }
        return sb.toString();
    }

    public String getLabel() {
        StringBuffer sb = new StringBuffer();

        if (set.size() == 1) {
            sb.append(EscapeUtils.escapeSpecialCharacters(set.iterator().next
                    ().getLabel()));
        } else {
            for (Element n : set) {
                if (sb.length() != 0) {
                    sb.append(",");
                }
                sb.append(EscapeUtils.escapeSpecialCharacters(n.getLabel()));
            }
        }
        return sb.toString();
    }


    public Collection<Element> getElements() {
        return set;
    }

    public boolean isSingleton() {
        return set.size() == 1;
    }

    public void addElement(Element n) {
        set.add(n);
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
        return getLabel().hashCode();
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
        return this.getLabel();
    }

    public boolean hasOverlap(EquiClass other) {
        return !intersection(other).isEmpty();
    }

    public EquiClass intersection(EquiClass other) {
        Set<Element> isect = new HashSet<>(set);

        isect.retainAll(other.set);

        EquiClass eisect = new EquiClass(isect);
        //LOGGER.debug("isect {} {} {}", this, other, eisect);
        return eisect;
    }

    public EquiClass union(EquiClass other) {
        Set<Element> union = new HashSet<>(set);
        union.addAll(other.set);
        return new EquiClass(union);
    }

    public EquiClass minus(EquiClass other) {
        Set<Element> minus = new HashSet<>(set);
        minus.removeAll(other.set);
        return new EquiClass(minus);
    }

    public boolean subsumes(EquiClass other) {
        return intersection(other).equals(other);
    }

    public boolean isAtomic() {
        return isSingleton();
    }


    public int getCardinality() {
        return this.set.size();
    }


    private Collection<EquiClass> singletonSplit() {

        assert isSingleton();

        List<EquiClass>ret = new Vector<>();

        for(Element e : set) {
            //LOGGER.debug("split {} {}", e, e.getClass().toGenericString());
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

    public Element getCorrespondingElement(Node n) {
        Predicate<Element> f;
        if(n.isOperation()) {
            f = e -> e.getAnnotation().equals(n.getLabel());
        } else {
            f = e -> e.getLabel().equals(n.getLabel());
        }

        Element ele = null;

        try {
            ele =  set.stream().filter(f).findFirst().get();
        } catch (NoSuchElementException e) {
            assert false;
        }
        return ele;
    }


    public EquiClass clone() {
        return new EquiClass(this);
    }

    /**
     * Infer additional facts from the ones present in this set
     * @return
     */
    public Set<EquiClass> infer() {

        LOGGER.debug("Infer additional facts for {} with set size {}", getDotLabel(),
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

        //LOGGER.debug("rows {}, cols {}", rows, cols);

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
            //LOGGER.debug("new fact {}", ec);
        }

        return ret;
    }

    public String debug() {
        StringBuilder sb = new StringBuilder();

        sb.append("==== EQ START:\n");

        for(Element e : set) {
            sb.append(e.getLabel() + "\n");
        }

        sb.append("==== EQ END");


        return sb.toString();

    }

}
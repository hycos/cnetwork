package org.snt.cnetwork.core.mergelattice;

import org.snt.cnetwork.utils.EscapeUtils;

import java.util.*;

public class EquiClass {
    Set<Element> set = new TreeSet<>();

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
        Set<Element> isect = new HashSet<>();
        isect.addAll(set);
        isect.retainAll(other.set);
        return new EquiClass(isect);
    }


    public boolean subsumes(EquiClass other) {
        return intersection(other).equals(other);
    }

    public int getCardinality() {
        return this.set.size();
    }


    public Collection<EquiClass> split () {
        Set ret = new LinkedHashSet<EquiClass>();

        if(set.size() > 1) {
            for (Element s : set) {
                //if (s.isSplittable()) {
                //    ret.add(new EquiClass(s.split()));
                //} else {
                    ret.add(new EquiClass(s));
                //}
            }
        } else if (set.size() == 1) {
                Element fst = this.set.iterator().next();
                if(fst.isTuple()) {
                    for(Element e : fst.split()) {
                        ret.add(new EquiClass(e));
                    }
                }
        }
        return ret;
    }

}
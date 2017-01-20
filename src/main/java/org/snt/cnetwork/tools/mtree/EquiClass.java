package org.snt.cnetwork.tools.mtree;

import org.snt.cnetwork.utils.EscapeUtils;

import java.util.*;

public class EquiClass<T extends Element> {
    Set<T> set = new LinkedHashSet<>();

    public static class Top extends EquiClass {
        @Override
        public String toString() {
            return "\u22A4";
        }
    }

    public static class Bottom extends EquiClass {
        @Override
        public String toString() {
            return "\u22A5";
        }
    }

    private static int nid = 0;
    private int id = 0;

    public EquiClass() {
        this.id = nid++;
    }

    public EquiClass(T nod) {
        this.id = nid++;
        set.add(nod);
    }

    public EquiClass(Collection<T> nods) {
        this.id = nid++;
        set.addAll(nods);
    }

    public String getDotLabel() {
        StringBuffer sb = new StringBuffer();
        sb.append("\\{");
        for(T n : set) {
            if(sb.charAt(sb.length()-1) != '{') {
                sb.append(",");
            }
            sb.append(EscapeUtils.escapeSpecialCharacters(n.getLabel()));
        }
        sb.append("\\}");
        return sb.toString();
    }

    protected Map<T, EquiClass<T>> convertToMap() {
        Map<T, EquiClass<T>> mp = new HashMap<>();
        for(T n : set) {
            mp.put(n, this);
        }
        return mp;
    }

    public Set<T> getElements() {
        return set;
    }

    public boolean isSingleton() {
        return set.size() == 1;
    }

    public void addElements(Set<T> nodes) {
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


    public Collection<EquiClass<T>> split () {
        Set ret = new LinkedHashSet<EquiClass<T>>();

        if(set.size() > 1) {
            for (T s : set) {
                //if (s.isSplittable()) {
                //    ret.add(new EquiClass(s.split()));
                //} else {
                    ret.add(new EquiClass(s));
                //}
            }
        } else if (set.size() == 1) {
                T fst = this.set.iterator().next();
                if(fst.isSplittable()) {
                    for(Element e : fst.split()) {
                        ret.add(new EquiClass(e));
                    }
                }
        }
        return ret;
    }
}
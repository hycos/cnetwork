package org.snt.cnetwork.utils;

import java.util.HashMap;
import java.util.Map;

/**
 * Simple implementation of a bi directional map
 * @param <K>
 * @param <V>
 */
public class BiMap<K,V> {

    protected Map<K,V> keytoval = new HashMap<>();
    protected Map<V,K> valtokey = new HashMap<>();

    public BiMap(){

    }

    public BiMap(BiMap other) {
        this.keytoval.putAll(other.keytoval);
        this.valtokey.putAll(other.valtokey);
    }

    public void put(K key, V val) {
        keytoval.put(key, val);
        valtokey.put(val, key);
    }

    public K getKeyByValue(V v) {
        return valtokey.get(v);
    }

    public V getValueByKey(K k) {
        return keytoval.get(k);
    }

    public boolean containsKey(K k) {
        return this.keytoval.containsKey(k);
    }

    public boolean containsValue(V v) {
        return this.valtokey.containsKey(v);
    }


    @Override
    public String toString() {


        StringBuilder sb = new StringBuilder();

        sb.append("keytoval: =======\n");
        for (Map.Entry<K, V> e : keytoval.entrySet()) {
            sb.append(" .. ");
            sb.append(e.getKey());
            sb.append(" -- ");
            sb.append(e.getValue());
            sb.append("\n");
        }
        sb.append("==================\n");
        sb.append("valtokey: =======\n");


        for (Map.Entry<V, K> e : valtokey.entrySet()) {
            sb.append(" .. ");
            sb.append(e.getKey());
            sb.append(" -- ");
            sb.append(e.getValue());
            sb.append("\n");
        }

        sb.append("==================\n");
        return sb.toString();

    }

}

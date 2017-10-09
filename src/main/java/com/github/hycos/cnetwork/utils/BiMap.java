/*
 * cnetwork - a constraint network implementation for Java
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetwork is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetwork.utils;

import java.util.HashMap;
import java.util.Map;

/**
 * Simple implementation of a bi directional map
 * @param <K> key
 * @param <V> value
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

    public void removeEntry (K k) {
        V val = keytoval.get(k);
        keytoval.remove(k);
        valtokey.remove(val);
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

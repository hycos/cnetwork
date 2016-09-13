package org.snt.cnetwork.core;

/**
 * Created by julian on 17/06/15.
 */
public interface NetworkEntity {


    interface NetworkEntityKind {
        public String toString();
        public String getValue();
        public int getId();
    }

    NetworkEntityKind getKind();


}

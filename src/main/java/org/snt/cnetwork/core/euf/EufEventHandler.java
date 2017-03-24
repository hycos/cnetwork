package org.snt.cnetwork.core.euf;


public interface EufEventHandler {
    /**
     * a callback method that is invoked whenever a new equiclass
     * is added to the euf lattice
     * @param ec
     */
    void onEquiClassAddition(EquiClass ec);
}

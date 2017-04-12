package org.snt.cnetwork.core.euf;


import org.snt.cnetwork.exception.EUFInconsistencyException;

public interface EufEventHandler {
    /**
     * a callback method that is invoked whenever a new equiclass
     * is added to the euf lattice
     * @param ec
     */
    void onEquiClassAddition(EquiClass ec) throws EUFInconsistencyException;
}

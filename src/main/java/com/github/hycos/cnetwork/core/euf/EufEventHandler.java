package com.github.hycos.cnetwork.core.euf;


import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

import java.util.Set;

public interface EufEventHandler {
    /**
     * a callback method that is invoked whenever a new equiclass
     * is added to the euf lattice
     * @param ec
     */
    void onEquiClassAddition(EquiClass ec) throws EUFInconsistencyException;

    /**
     * a callback method that is invoked whenever an equiclass is replaced
     * @param toReplace
     * @param replacement
     * @throws EUFInconsistencyException
     */
    void onEquiClassReplace(Set<EquiClass> toReplace, EquiClass replacement)
            throws
            EUFInconsistencyException;

}

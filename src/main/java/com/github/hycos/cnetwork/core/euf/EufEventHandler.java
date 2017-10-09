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
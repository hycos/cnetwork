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

package com.github.hycos.cnetwork.core.graph;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

import java.util.HashSet;
import java.util.Set;

public class ConstraintNetworkSubject<T> {

    final static Logger LOGGER = LoggerFactory.getLogger(ConstraintNetworkSubject.class);

    private Set<ConstraintNetworkObserver<T>> observers = new
            HashSet<>();

    public void attach(ConstraintNetworkObserver observer){
        observers.add(observer);
    }

    public void notifyAllObservers(T item) throws EUFInconsistencyException {
        LOGGER.debug("notify all observers {}", observers.size());
        for (ConstraintNetworkObserver observer : observers) {
            observer.update(item);
        }
    }

}
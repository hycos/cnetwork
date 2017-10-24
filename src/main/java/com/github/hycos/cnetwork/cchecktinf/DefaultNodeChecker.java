/*
 * polyglot - translate constraints in between different formats
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * polyglot is licensed under the EUPL, Version 1.1 or – as soon
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

package com.github.hycos.cnetwork.cchecktinf;

import com.github.hycos.cnetwork.api.NodeInterface;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkInterface;
import com.github.hycos.cnetwork.api.cchecktinf.AbstractConsistencyChecker;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class DefaultNodeChecker extends AbstractConsistencyChecker {

    Set<Integer> arities = new HashSet<>();

    public DefaultNodeChecker(int ... narity) {
        arities = Arrays.stream(narity)
                .boxed()
                .collect(Collectors.toSet());
    }

    @Override
    public boolean check(ConstraintNetworkInterface cb, NodeInterface n) {
        for(int a : arities){
            if(arities.size() == 1 && a == 0)
                return true;

            if(arities.size() == 1 && a == -1)
                return cb.getParametersFor(n).size() >= 2;

            if(super.checkNary(cb, n, a))
                return true;
        }
        return false;
    }
}

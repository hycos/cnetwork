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

package com.github.hycos.cnetwork.core.consistency;

import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;

import java.util.List;
import java.util.function.Predicate;

/**
 * An interface for checking consistency of operations and
 * for type inference
 */
public abstract class ConsistencyChecker {
    public abstract boolean check(ConstraintNetworkBuilder cb, Node n);

    protected boolean checkNary(ConstraintNetworkBuilder cb,
                                Node op,
                                int arity,
                                Predicate<Node> paramPredicate,
                                Predicate<Node> opPredicate) {

        List<Node> params = cb.getParametersFor(op);

        if(arity > 0) {
            if(params.size() != arity)
                return false;
        }

        return params.stream().filter(paramPredicate).count() == params.size
                () && opPredicate.test(op);

    }

    protected boolean checkNaryPlus2(ConstraintNetworkBuilder cb,
                                Node op,
                                Predicate<Node> paramPredicate,
                                Predicate<Node> opPredicate) {

        List<Node> params = cb.getParametersFor(op);

        if(params.size() < 2) {
            return false;
        }


        return params.stream().filter(paramPredicate).count() == params.size
                () && opPredicate.test(op);

    }

}

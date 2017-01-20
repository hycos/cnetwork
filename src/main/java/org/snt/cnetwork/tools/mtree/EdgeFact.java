/*
* prex - approximate regular expression matching
*
* Copyright 2016, Julian Thomé <julian.thome@uni.lu>
*
* Licensed under the EUPL, Version 1.1 or – as soon they will be approved by
* the European Commission - subsequent versions of the EUPL (the "Licence");
* You may not use this work except in compliance with the Licence. You may
* obtain a copy of the Licence at:
*
* https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the Licence is distributed on an "AS IS" basis, WITHOUT
* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the Licence for the specific language governing permissions and
* limitations under the Licence.
*/

package org.snt.cnetwork.tools.mtree;


import org.jgrapht.EdgeFactory;

public class EdgeFact<T extends Element> implements EdgeFactory<EquiClass<T>,
        EquiEdge> {

    @Override
    public EquiEdge createEdge(EquiClass<T> sourceVertex,
                                             EquiClass<T> targetVertex) {
        return new EquiEdge(sourceVertex, targetVertex, EquiEdge.Kind.EQUI,-1);
    }
}
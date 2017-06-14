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

package org.snt.cnetwork.core.execdag;


import org.jgrapht.EdgeFactory;
import org.snt.cnetwork.core.graph.Node;

public class ExecEdgeFact implements EdgeFactory<Node,ExecEdge> {

    @Override
    public ExecEdge createEdge(Node sourceVertex,
                                             Node targetVertex) {
        return new ExecEdge(sourceVertex, targetVertex, 0);
    }
}
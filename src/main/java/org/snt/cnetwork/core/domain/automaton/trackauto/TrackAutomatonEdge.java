
package org.snt.cnetwork.core.domain.automaton.trackauto;

import org.jgrapht.graph.DefaultEdge;

public class TrackAutomatonEdge extends DefaultEdge {

    @Override
    public TrackAutomatonNode getSource() {
        return (TrackAutomatonNode)super.getSource();
    }

    @Override
    public TrackAutomatonNode getTarget() {
        return (TrackAutomatonNode)super.getTarget();
    }

}

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

package com.github.hycos.cnetwork.core;

import com.github.hycos.domctrl.Domain;
import com.github.hycos.domctrl.DomainControllerInterface;
import com.github.hycos.domctrl.exception.DomainControllerException;
import com.github.hycos.cnetwork.api.labelmgr.ConstraintNetworkInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.Node;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

public class DefaultDomainController implements
        DomainControllerInterface<Node> {

    private Map<Node, Domain> dmap =  new HashMap();
    private ConstraintNetworkInterface c = null;

    final static Logger LOGGER = LoggerFactory.getLogger(DefaultDomainController.class);

    public DefaultDomainController(DefaultDomainController other) {
        this.dmap.putAll(other.dmap);
    }

    public DefaultDomainController() {

    }

    @Override
    public Domain getDomain(Node n) throws DomainControllerException {
        assert(dmap.containsKey(n));
        LOGGER.debug("get domain");
        return dmap.get(n);
    }

    @Override
    public void setDomain(Node n, Domain dom) throws
            DomainControllerException {
        LOGGER.debug("set domain");
        dmap.put(n,dom);
    }

    @Override
    public boolean hasDomain(Node n) {
        return dmap.containsKey(n);
    }

    @Override
    public Domain createDomainFor(Node n) {
        return dmap.put(n, new DefaultDomain(n,false));
    }

    @Override
    public DomainControllerInterface clone() {
        return new DefaultDomainController(this);
    }

    @Override
    public void onNodeCollapse(Node toReplace, Node replacement) throws InconsistencyException {

    }

    @Override
    public void onNodeDelete(Node deleted) {

    }

    @Override
    public void onNodeAdd(Node n, boolean isConstraint) {
        LOGGER.info("on on operation add {} {} {}", n.getShortLabel(), n
                        .getId(),
                isConstraint);


        if(dmap.containsKey(n))
            return;

        assert !(n.getId() == 31 && isConstraint == false);


        n.setDomainController(this);
        dmap.put(n, new DefaultDomain(n,isConstraint));

        if(isConstraint)
            assert n.getDomain().isConstraint();
    }

    @Override
    public void onConstraintAdd(Node n) throws InconsistencyException {
        //dmap.replace(n, new DefaultDomain(n,true));
    }

    @Override
    public void onConnectionAdd(Node frst, Node snd) {

    }

    @Override
    public void update(Node n) throws InconsistencyException {

    }

    @Override
    public void attach(Node n) {
        n.attach(this);
    }

    @Override
    public void register(ConstraintNetworkInterface<Node> c) {
        this.c = c;
    }
}

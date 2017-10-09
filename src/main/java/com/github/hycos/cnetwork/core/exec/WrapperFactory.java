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

package com.github.hycos.cnetwork.core.exec;

import com.github.hycos.cnetwork.core.graph.Operation;
import com.github.hycos.cnetwork.sig.JavaMethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;

public class WrapperFactory {

    private static WrapperFactory instance = null;

    private HashMap<JavaMethodSignature, UnknownFunctionExecutor> invocators = null;


    final static Logger LOGGER = LoggerFactory.getLogger(WrapperFactory.class);

    public static WrapperFactory getInstance() {
        if(instance == null)
            instance = new WrapperFactory();
        return instance;
    }

    private WrapperFactory() {
        invocators = new HashMap<>();
    }

    public UnknownFunctionExecutor getInvocator(Operation n) {
        return getInvocator(n.getSig());
    }

    public UnknownFunctionExecutor getInvocator(JavaMethodSignature sig) {

        LOGGER.debug("look for {}", sig.toBCString());

        if(!this.invocators.containsKey(sig)) {
            this.invocators.put(sig, new UnknownFunctionExecutor(sig));
        }

        return this.invocators.get(sig);
    }

    public Object invoke(JavaMethodSignature sig, Object ... objects) {
        if(!this.invocators.containsKey(sig)) {
            this.invocators.put(sig, new UnknownFunctionExecutor(sig));
        }

        return this.invocators.get(sig).invoke(objects);
    }

}

package org.snt.cnetwork.core.exec;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.Operation;
import org.snt.cnetwork.sig.JavaMethodSignature;

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

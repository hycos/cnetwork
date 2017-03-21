package org.snt.cnetwork.core.exec;

import org.snt.cnetwork.core.Operation;
import org.snt.cnetwork.sig.JavaMethodSignature;

import java.util.HashMap;

public class WrapperFactory {

    private static WrapperFactory instance = null;

    private HashMap<JavaMethodSignature, UnknownFunctionExecutor> invocators = null;


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

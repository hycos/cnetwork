package com.github.hycos.cnetwork.core.exec;

import com.github.hycos.cnetwork.sig.JavaMethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import java.util.List;
import java.util.Vector;

public class MethodChain implements FunctionInvocator {

    List<UnknownFunctionExecutor> inv = null;

    final static Logger LOGGER = LoggerFactory.getLogger(MethodChain.class);

    public MethodChain() {
        this.inv = new Vector<>();
    }

    private Object ret = null;

    public MethodChain(List<JavaMethodSignature> mc) {
        this();
        for(JavaMethodSignature sig : mc) {
            inv.add(WrapperFactory.getInstance().getInvocator(sig));
        }
    }

    public void addMethod(JavaMethodSignature sig) {
        inv.add(WrapperFactory.getInstance().getInvocator(sig));
    }

    public void addMethod(String sig) {
        inv.add(WrapperFactory.getInstance().getInvocator(JavaMethodSignature.fromString(sig)));
    }

    public Object invoke(Object ... args) {

        Object arg = null;
        for(int i = 0; i < inv.size(); i++ ){
            if(i == 0) {
                arg = inv.get(i).invoke(args);
            } else {
                arg = inv.get(i).invoke(arg);
            }
        }
        return arg;
    }

    @Override
    public void execute(Object... params) {
        this.ret = invoke(params);
    }

    @Override
    public Object[] getReturnValues() {
        Object [] ret = new Object [] {this.ret};
        return ret;
    }

}

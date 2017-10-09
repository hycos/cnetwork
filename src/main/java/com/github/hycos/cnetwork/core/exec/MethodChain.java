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

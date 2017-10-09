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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.sig.JavaMethodSignature;
import com.github.hycos.cnetwork.sig.JavaType;


import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;


public class UnknownFunctionExecutor implements Executor {

    final static Logger LOGGER = LoggerFactory.getLogger(UnknownFunctionExecutor.class);

    private Object o;
    private Class c;
    private Method m;


    public UnknownFunctionExecutor(JavaMethodSignature sig) {
        Class c = null;
        LOGGER.debug("Load " + sig.getDeclaringType().toHRString());
        try {
            c = Class.forName(sig.getDeclaringType().toHRString());
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }

        Class[] args = new Class[sig.getArgumentTypes().size()];

        int idx = 0;
        for (JavaType jt : sig.getArgumentTypes()) {
            try {
                args[idx++] = Class.forName(jt.toHRString());
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            }
        }

        assert (c != null);

        Object o = null;

        try {
            o = c.newInstance();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }


        try {
            m = c.getMethod(sig.getMethodName(), args);
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }

        assert (m != null);

    }

    public Object invoke (Object ... args) {

        Object out = "";

        try {
            out = m.invoke(o, args);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
            LOGGER.error(e.getMessage());
        } catch (InvocationTargetException e) {
            e.printStackTrace();
            LOGGER.error(e.getMessage());
        }
        //LOGGER.info("RETOUT " + out);
        assert(out instanceof String);
        return out;
    }




}

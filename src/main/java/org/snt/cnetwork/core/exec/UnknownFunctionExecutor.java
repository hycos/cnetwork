package org.snt.cnetwork.core.exec;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.sig.JavaMethodSignature;
import org.snt.cnetwork.sig.JavaType;


import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;


public class UnknownFunctionExecutor implements Executor {

    final static Logger LOGGER = LoggerFactory.getLogger(UnknownFunctionExecutor.class);

    private Object o;
    private Class c;
    private Method m;


    public UnknownFunctionExecutor(JavaMethodSignature sig) {
        Class c = null;
        //LOGGER.info("Load " + sig.getDeclaringType().toHRString());
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

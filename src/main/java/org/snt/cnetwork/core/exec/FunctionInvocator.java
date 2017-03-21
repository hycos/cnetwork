package org.snt.cnetwork.core.exec;

public interface FunctionInvocator {
    void execute(Object ... params);
    Object[] getReturnValues();
}

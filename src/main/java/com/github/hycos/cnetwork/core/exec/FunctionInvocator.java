package com.github.hycos.cnetwork.core.exec;

public interface FunctionInvocator {
    void execute(Object ... params);
    Object[] getReturnValues();
}

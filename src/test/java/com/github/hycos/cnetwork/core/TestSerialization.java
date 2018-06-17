package com.github.hycos.cnetwork.core;

import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import com.github.hycos.cnetwork.tools.io.Serializer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestSerialization {

    final static Logger LOGGER = LoggerFactory.getLogger(TestTypeInference.class);

    @Test
    public void testSerialize() {


        ConstraintNetworkBuilder cb = new ConstraintNetworkBuilder();
        cb.addOperand(DefaultNodeKind.STRVAR, "a");

        byte [] b = Serializer.INSTANCE.toByteArray(cb.getConstraintNetwork());

        ConstraintNetwork nn = Serializer.INSTANCE.fromByteArray(b);
        
        Assertions.assertEquals(nn.vertexSet(), cb.vertexSet());

    }
}

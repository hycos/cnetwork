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

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.graph.Operand;


public class TestBooleanRange {

    final static Logger LOGGER = LoggerFactory.getLogger(TestBooleanRange.class);

    @Test
    public void testBooleanRange() {
        Operand otrue = new Operand("true", NodeKind.BOOLLIT);
        Operand ofalse = new Operand("false", NodeKind.BOOLLIT);
        Operand ovar = new Operand("v3", NodeKind.BOOLVAR);

        LOGGER.info(otrue.toString());
        LOGGER.info(ofalse.toString());
        LOGGER.info(ovar.toString());
    }
}


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

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.domain.automaton.SimpleAutomaton;


public class TestAutomaton {

    final static Logger LOGGER = LoggerFactory.getLogger(TestAutomaton.class);

    @Test
    public void testsimple() {

        SimpleAutomaton a0 = new SimpleAutomaton("abcded");
        Assert.assertTrue(a0.isSingleton());

        LOGGER.debug(a0.getShortestExample());

        SimpleAutomaton a1 = new SimpleAutomaton("abcde+d");
        Assert.assertFalse(a1.isSingleton());

        LOGGER.debug(a1.getShortestExample());

        SimpleAutomaton a3 = new SimpleAutomaton("");
        Assert.assertTrue(a3.isSingleton());
        Assert.assertTrue(a3.isEmptyString());


        SimpleAutomaton a4 = new SimpleAutomaton("hello w(orld)*");
        Assert.assertFalse(a4.isSingleton());
        Assert.assertFalse(a4.isEmptyString());

        SimpleAutomaton a5 = a4.intersect(new SimpleAutomaton("hello w"));
        Assert.assertTrue(a5.isSingleton());
        Assert.assertFalse(a5.isEmptyString());


    }

}


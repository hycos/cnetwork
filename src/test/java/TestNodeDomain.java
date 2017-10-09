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
import com.github.hycos.cnetwork.core.domain.NodeDomain;
import com.github.hycos.cnetwork.core.domain.NodeDomainFactory;

public class TestNodeDomain {

    final static Logger LOGGER = LoggerFactory.getLogger(TestDomainUtils.class);

    @Test
    public void testCnConstruction() {

        NodeDomain dbfalse = NodeDomainFactory.DBFALSE;
        NodeDomain dbtrue = NodeDomainFactory.DBTRUE;
        NodeDomain dbtrue2 = NodeDomainFactory.DBTRUE;
        NodeDomain db = NodeDomainFactory.DB;

        NodeDomain str = NodeDomainFactory.DSTR;
        NodeDomain strt = NodeDomainFactory.DSTRT;

        Assert.assertNotEquals(dbfalse, dbtrue);
        Assert.assertEquals(dbtrue2, dbtrue);
        Assert.assertEquals(dbtrue, dbtrue);
        Assert.assertEquals(dbfalse,dbfalse);

        Assert.assertNotEquals(db, dbtrue);
        Assert.assertNotEquals(db, dbfalse);
        Assert.assertEquals(db,db);

        Assert.assertNotEquals(str, strt);
        Assert.assertEquals(str,str);
        Assert.assertEquals(strt,strt);

    }
}

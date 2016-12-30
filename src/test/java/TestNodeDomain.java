import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.NodeDomain;
import org.snt.cnetwork.core.domain.NodeDomainFactory;

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

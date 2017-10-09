import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.domain.range.AboveAll;
import com.github.hycos.cnetwork.core.domain.range.BelowAll;
import com.github.hycos.cnetwork.core.domain.range.NumCut;


public class TestCut {

    final static Logger LOGGER = LoggerFactory.getLogger(TestCut.class);


    @Test
    public void testNumCut() {

        NumCut nc1 = new NumCut(100L);
        NumCut nc2 = new NumCut(101L);
        NumCut nc3 = new NumCut(101L);
        NumCut above1 = new AboveAll();
        NumCut above2 = new AboveAll(1L);
        NumCut above3 = new AboveAll(-1L);

        NumCut below1 = new BelowAll();
        NumCut below2 = new BelowAll(1L);
        NumCut below3 = new BelowAll(-1L);


        Assert.assertTrue(nc1.isSmallerThan(nc2));
        Assert.assertTrue(nc1.isSmallerEqualsThan(nc2));
        Assert.assertTrue(nc2.isGreaterThan(nc1));
        Assert.assertTrue(nc2.isGreaterEqualsThan(nc2));
        Assert.assertFalse(nc1.equals(nc2));
        Assert.assertTrue(nc3.equals(nc2));
        Assert.assertTrue(nc3.equals(nc3));

        Assert.assertTrue(above1.isGreaterEqualsThan(nc1));
        Assert.assertTrue(above1.isGreaterEqualsThan(nc2));
        Assert.assertTrue(above1.isGreaterEqualsThan(nc3));
        Assert.assertTrue(above2.isGreaterEqualsThan(above1));
        Assert.assertTrue(above3.isSmallerEqualsThan(above1));
        Assert.assertTrue(above1.isAboveAll());
        Assert.assertFalse(above1.isBelowAll());
        Assert.assertFalse(above1.isFixed());

        Assert.assertTrue(nc1.isSmallerThan(above1));
        Assert.assertTrue(nc2.isSmallerThan(above1));
        Assert.assertTrue(nc3.isSmallerThan(above1));


        Assert.assertNotEquals(above1, above2);
        Assert.assertTrue(above2.isGreaterThan(above1));
        Assert.assertTrue(below1.isSmallerThan(below2));
        Assert.assertTrue(below3.isSmallerThan(below2));
        Assert.assertTrue(below1.isBelowAll());
        Assert.assertFalse(below1.isAboveAll());
        Assert.assertFalse(below1.isFixed());


        LOGGER.debug("min {}", nc1.compareTo(nc2));

    }
}

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.AtomicNumRange;
import org.snt.cnetwork.core.domain.BooleanRange;
import org.snt.cnetwork.core.domain.NumRange;
import org.snt.cnetwork.utils.RexpUtils;

import java.util.Set;


public class TestBasicRange {

    final static Logger LOGGER = LoggerFactory.getLogger(TestBasicRange.class);

    public static BooleanRange trange = new BooleanRange(BooleanRange.BooleanValue.TRUE);
    public static BooleanRange frange = new BooleanRange(BooleanRange.BooleanValue.FALSE);
    public static BooleanRange vrange = new BooleanRange();


    private void debug(Set<AtomicNumRange> nr) {
        for(AtomicNumRange n : nr) {
            LOGGER.info(">> " + n.toString());
        }
    }

    @Test
    public void testMinus() {

        AtomicNumRange nr0 = new AtomicNumRange(0,100);
        AtomicNumRange nr1 = new AtomicNumRange(50,99);

        NumRange nset0 = nr0.minus(nr1);

        assert(nset0.size() == 2);

        for(AtomicNumRange n : nset0.getRangeMap().values()) {
            assert(n.equals(0,49) || n.equals(100,100));
        }

        AtomicNumRange nr2 = new AtomicNumRange(150, 190);
        AtomicNumRange nr3 = new AtomicNumRange(190, 200);

        NumRange nset1 = nr2.minus(nr3);
        NumRange nset2 = nr3.minus(nr2);

        //debug(nset1);
        //debug(nset2);

        assert(nset1.size() == 1);
        assert(nset2.size() == 1);


        Assert.assertEquals(nset1.getMin(), 150);
        Assert.assertEquals(nset1.getMax(), 189);

        Assert.assertEquals(nset2.getMin(), 191);
        Assert.assertEquals(nset2.getMax(), 200);



        AtomicNumRange nr4 = new AtomicNumRange(1,1000);
        AtomicNumRange nr5 = new AtomicNumRange(0,2000);

        NumRange nset3 = nr4.minus(nr5);
        NumRange nset4 = nr5.minus(nr4);

        assert(nset3 == null);

        for(AtomicNumRange n : nset4.getRangeMap().values()) {
            assert(n.equals(0,0) || n.equals(1001,2000));
        }

    }

    @Test
    public void testBooleanRange() {

        AtomicNumRange r1 = new AtomicNumRange(2014, 3050);
        AtomicNumRange r2 = new AtomicNumRange(1000, 3050);

        AtomicNumRange isect = r1.intersect(r2);

        assert(r1.intersect(r2) != null);
        assert(r1.getMin() == 2014 && r1.getMax() == 3050);

        assert(trange.isAlwaysTrue());
        assert(frange.isAlwaysFalse());
        assert(!trange.isCatState());
        assert(!frange.isCatState());

        assert(trange.and(trange).isAlwaysTrue());
        assert(!trange.and(frange).isAlwaysTrue());
        assert(trange.and(frange).isAlwaysFalse());
        assert(frange.and(frange).isAlwaysFalse());

        assert(trange.or(trange).isAlwaysTrue());
        assert(trange.or(frange).isAlwaysTrue());
        assert(trange.or(frange).isAlwaysTrue());
        assert(frange.or(frange).isAlwaysFalse());

        LOGGER.debug("" + frange.xor(trange));

        assert(frange.xor(trange).isAlwaysTrue());


        BooleanRange ntrange = trange.negate();

        assert(ntrange.equals(frange));

        trange = ntrange.negate();

        BooleanRange nfrange = frange.negate();

        assert(trange.equals(nfrange));

        BooleanRange frange = nfrange.negate();

        assert(vrange.and(trange).isCatState());
        assert(vrange.and(frange).isAlwaysFalse());

        assert(vrange.or(trange).isAlwaysTrue());
        assert(vrange.or(frange).isCatState());

    }


    @Test
    public void testsimple() {
        //BasicRange r1 = new BasicRange(-2147483643,  2147483643);
        LOGGER.info(RexpUtils.getRexpForRange(-2147483643,  2147483643));

    }

}


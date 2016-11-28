import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.domain.AtomicNumRange;
import org.snt.cnetwork.core.domain.NodeDomainFactory;
import org.snt.cnetwork.core.domain.NumRange;


public class TestNumRange {

    final static Logger LOGGER = LoggerFactory.getLogger(TestNumRange.class);

    private void assertion(NumRange rs, int size, long floorEntry, long floorEntryMax, long ceilEntry, long ceilEntryMax) {
        LOGGER.info(rs.toString());
        assert(rs.size() == size);
        assert(rs.getRangeMap().floorEntry(floorEntry).getValue().getMax() == floorEntryMax);
        assert(rs.getRangeMap().ceilingEntry(ceilEntry).getValue().getMax() == ceilEntryMax);
    }

    @Test
    public void testUnion0() {
        NumRange rs = new NumRange();
        rs.add(new AtomicNumRange(170,200));
        rs.add(new AtomicNumRange(210,220));
        rs.add(new AtomicNumRange(180,210));
        assertion(rs, 1, 170L, 220L, 170L, 220L);
    }

    @Test
    public void testUnion1() {

        NumRange rs = new NumRange();
        rs.add(new AtomicNumRange(5,100));
        rs.add(new AtomicNumRange(160,180));
        assertion(rs,2, 5L, 100L, 160L, 180L);
        rs.add(new AtomicNumRange(200,225));
        assertion(rs,3, 5L, 100L, 200L, 225L);
        rs.add(new AtomicNumRange(300,335));
        assertion(rs,4, 5L, 100L, 300L, 335L);
        rs.add(new AtomicNumRange(370,400));
        assertion(rs,5, 5L, 100L, 370L, 400L);
        //5 [5,100]
        //160 [160,180] *
        //200 [200,225]
        //300 [300,335] *
        //370 [370,400]
        rs.add(new AtomicNumRange(155,340));
        assertion(rs,3, 5L, 100L, 370L, 400L);
        //5 [5,100]
        //160 [160,340]
        //370 [370,400]
        rs.add(new AtomicNumRange(101,369));
        assertion(rs, 1, 5L, 400L, 0L, 400L);
    }

    @Test
    public void testUnion2() {
        NumRange rs = new NumRange();
        rs.add(new AtomicNumRange(-100,100));
        rs.add(new AtomicNumRange(-101,102));
        rs.add(new AtomicNumRange(-103,102));
        assertion(rs, 1, -103L, 102L, -103L, 102L);
        rs.add(new AtomicNumRange(-200, -157));
        assertion(rs, 2, -200L, -157L, -103L, 102L);
        rs.add(new AtomicNumRange(-300, -201));
        assertion(rs, 2, -300, -157L, -103L, 102L);
        rs.add(new AtomicNumRange(-500, 500));
        assertion(rs, 1, -500L, 500L, -500L, 500L);
        rs.add(new AtomicNumRange(-501, 500));
        assertion(rs, 1, -501L, 500L, -501L, 500L);
        rs.add(new AtomicNumRange(-501, 501));
        assertion(rs, 1, -501L, 501L, -501L, 501L);
    }

    @Test
    public void testUnion3() {
        NumRange rs = new NumRange();
        rs.add(new AtomicNumRange(1,1));
        rs.add(new AtomicNumRange(2,2));
        rs.add(new AtomicNumRange(3,3));
        rs.add(new AtomicNumRange(4,4));
        rs.add(new AtomicNumRange(5,5));
        rs.add(new AtomicNumRange(5,5));
        assertion(rs,1,1L,5L,1L,5L);
    }

    @Test
    public void testUnion4() {
        NumRange rs = new NumRange();
        rs.add(new AtomicNumRange(1,1));
        rs.add(new AtomicNumRange(3,3));
        rs.add(new AtomicNumRange(5,5));
        rs.add(new AtomicNumRange(7,7));
        rs.add(new AtomicNumRange(9,9));
        rs.add(new AtomicNumRange(11,11));
        rs.add(new AtomicNumRange(13,13));
        rs.add(new AtomicNumRange(15,15));
        rs.add(new AtomicNumRange(17,17));
        rs.add(new AtomicNumRange(19,19));
        rs.add(new AtomicNumRange(21,21));

        assertion(rs,11,1L,1L,21L,21L);
        rs.add(new AtomicNumRange(4,4));
        assertion(rs,10,1L,1L,21L,21L);
        rs.add(new AtomicNumRange(0,1));
        assertion(rs,10,0L,1L,21L,21L);
        rs.add(new AtomicNumRange(0,3));
        assertion(rs,9,0L,5L,21L,21L);
        rs.add(new AtomicNumRange(21,150));
        assertion(rs,9,0L,5L,21L,150L);
        rs.add(new AtomicNumRange(11,17));
        assertion(rs,6,0L,5L,21L,150L);
        rs.add(new AtomicNumRange(9,10));
        assertion(rs,5,0L,5L,21L,150L);
        rs.add(new AtomicNumRange(19,20));
        assertion(rs,4,0L,5L,19L,150L);
        rs.add(new AtomicNumRange(100,121));
        assertion(rs,4,0L,5L,19L,150L);
        rs.add(new AtomicNumRange(-300,300));
        assertion(rs,1,-300L,300L,-300L,300L);
        LOGGER.info(rs.toString());
    }

    @Test
    public void testUnion5() {
        NumRange rs = new NumRange();
        rs.add(new AtomicNumRange(10,20));
        rs.add(new AtomicNumRange(40,50));
        rs.add(new AtomicNumRange(70,80));
        rs.add(new AtomicNumRange(100,110));

        assertion(rs,4,10L,20L,100L,110L);
        rs.add(new AtomicNumRange(51,69));
        assertion(rs,3,10L,20L,100L,110L);
        rs.add(new AtomicNumRange(10,20));
        assertion(rs,3,10L,20L,100L,110L);
        rs.add(new AtomicNumRange(100,110));
        assertion(rs,3,10L,20L,100L,110L);

        LOGGER.info(rs.toString());
    }

    @Test
    public void testUnion6() {
        NumRange rs = new NumRange();
        rs.add(new AtomicNumRange(1,1));
        rs.add(new AtomicNumRange(3,3));
        rs.add(new AtomicNumRange(5,5));
        rs.add(new AtomicNumRange(7,7));
        rs.add(new AtomicNumRange(9,9));
        rs.add(new AtomicNumRange(11,11));
        rs.add(new AtomicNumRange(13,13));
        rs.add(new AtomicNumRange(15,15));
        rs.add(new AtomicNumRange(17,17));
        rs.add(new AtomicNumRange(19,19));
        rs.add(new AtomicNumRange(21,21));

        rs.add(new AtomicNumRange(4,4));

        LOGGER.info(rs.toString());
    }


    @Test
    public void testUnion7() {
        NumRange rs = new NumRange(NodeDomainFactory.Z);

        LOGGER.info(rs.toString());
        NumRange rs0 = new NumRange();
        rs0.add(new AtomicNumRange(0,100));
        LOGGER.info("++********* 0" + rs0.toString());

        NumRange rs1 = new NumRange();
        rs1.add(new AtomicNumRange(10,20));
        rs1.add(new AtomicNumRange(25,70));
        rs1.add(new AtomicNumRange(90,140));
        LOGGER.info("++********* 1" + rs1.toString());

        NumRange com = rs0.minus(rs1);
        assertion(com,3,0,9,71,89);
        NumRange check = rs0.minus(rs0);
        assert(check == null);
    }

    @Test
    public void testUnion8() {
        NumRange n0 = new NumRange();
        n0.add(new AtomicNumRange(1,1));
        n0.add(new AtomicNumRange(3,3));
        n0.add(new AtomicNumRange(5,5));
        n0.add(new AtomicNumRange(7,7));
        n0.add(new AtomicNumRange(9,9));

        NumRange n1 = new NumRange();
        n1.add(new AtomicNumRange(-1,-1));
        n1.add(new AtomicNumRange(3,5));


        LOGGER.info(n0.toString());
        LOGGER.info(n1.toString());

        NumRange add = n0.numadd(n1);
        LOGGER.info(add.toString());

    }


    @Test
    public void testIntersection0() {
        NumRange rs0 = new NumRange();

        rs0.add(new AtomicNumRange(1,1));
        rs0.add(new AtomicNumRange(3,3));
        rs0.add(new AtomicNumRange(5,5));
        rs0.add(new AtomicNumRange(7,7));
        rs0.add(new AtomicNumRange(9,9));

        NumRange rs1 = new NumRange();

        rs1.add(new AtomicNumRange(1,3));

        NumRange isect = rs0.intersect(rs1);

        assert(isect.getMin() == 1);
        assert(isect.getMax() == 3);

        assertion(isect, 2, 1L, 1L, 2L, 3L);

        LOGGER.info(isect.toString());
    }

    @Test
    public void testIntersection1() {
        NumRange rs0 = new NumRange();
        rs0.add(new AtomicNumRange(1,1));
        rs0.add(new AtomicNumRange(3,3));
        rs0.add(new AtomicNumRange(5,5));
        rs0.add(new AtomicNumRange(7,7));
        rs0.add(new AtomicNumRange(9,9));
        rs0.add(new AtomicNumRange(11,11));
        rs0.add(new AtomicNumRange(13,13));
        rs0.add(new AtomicNumRange(15,15));
        rs0.add(new AtomicNumRange(17,17));
        rs0.add(new AtomicNumRange(19,19));
        rs0.add(new AtomicNumRange(21,21));

        NumRange rs1 = new NumRange();
        rs1.add(new AtomicNumRange(1,1));
        rs1.add(new AtomicNumRange(3,3));
        rs1.add(new AtomicNumRange(5,5));
        rs1.add(new AtomicNumRange(7,7));
        rs1.add(new AtomicNumRange(9,9));
        rs1.add(new AtomicNumRange(11,11));
        rs1.add(new AtomicNumRange(13,13));
        rs1.add(new AtomicNumRange(15,15));
        rs1.add(new AtomicNumRange(17,17));
        rs1.add(new AtomicNumRange(19,19));
        rs1.add(new AtomicNumRange(21,21));

        NumRange isect = rs0.intersect(rs1);

        assert(rs1.equals(isect));

        NumRange rs2 = new NumRange();
        rs2.add(new AtomicNumRange(1,1));
        rs2.add(new AtomicNumRange(3,3));
        rs2.add(new AtomicNumRange(5,5));
        rs2.add(new AtomicNumRange(7,7));
        rs2.add(new AtomicNumRange(9,9));

        rs2.add(new AtomicNumRange(17,17));
        rs2.add(new AtomicNumRange(19,19));
        rs2.add(new AtomicNumRange(21,21));

        assert(rs2.intersect(rs1).equals(rs2));

    }

    @Test
    public void testIntersection2() {
        NumRange rs0 = new NumRange();
        rs0.add(new AtomicNumRange(1,100));
        rs0.add(new AtomicNumRange(300,350));
        rs0.add(new AtomicNumRange(500,5000));


        NumRange rs1 = new NumRange();
        rs1.add(new AtomicNumRange(-199,10));
        rs1.add(new AtomicNumRange(100,176));
        rs1.add(new AtomicNumRange(460,10000));

        NumRange isect = rs1.intersect(rs0);

        LOGGER.info(isect.toString());

    }

    @Test
    public void testSetMin() {
        NumRange rs0 = new NumRange();
        rs0.add(new AtomicNumRange(1,100));
        rs0.add(new AtomicNumRange(300,350));
        rs0.add(new AtomicNumRange(500,5000));


        NumRange rs1 = new NumRange();
        rs1.add(new AtomicNumRange(-199,10));
        rs1.add(new AtomicNumRange(100,176));
        rs1.add(new AtomicNumRange(460,10000));

        NumRange isect = rs1.intersect(rs0);
        //(1)[1,10],[100,176],[460,10000](10000)

        isect.setMin(11);

        LOGGER.info(isect.toString());

    }

    @Test
    public void testIsect() {
        NumRange rs0 = new NumRange();
        rs0.add(new AtomicNumRange(0,100));


        LOGGER.info("RS 0" + rs0.toString());
        NumRange rs1 = new NumRange();
        rs1.add(new AtomicNumRange(0,50));
        rs1.add(new AtomicNumRange(52,100));
        LOGGER.info("RS 1 " + rs1.toString());

        NumRange out = rs0.intersect(rs1);

        LOGGER.info(">> " + out.toString());

    }

    @Test
    public void testEqualIsect() {
        NumRange rs0 = new NumRange();
        rs0.add(new AtomicNumRange(36,36));

        NumRange rs1 = new NumRange();
        rs1.add(new AtomicNumRange(36,36));

        LOGGER.info("RS 1 " + rs1.toString());

        NumRange out = rs0.intersect(rs1);
        NumRange out2 = out.intersect(rs0);

        LOGGER.info(">> " + out2.toString());

    }

}


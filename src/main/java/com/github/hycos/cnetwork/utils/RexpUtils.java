package com.github.hycos.cnetwork.utils;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class RexpUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(RexpUtils.class);

    public static String getRexpForRange(long min, long max) {
        return getRexpForMin(min) + "&" + getRexpForMax(max);
    }


    public static String getRexpForMin(long min) {
        if(min < 0) {
            min *= -1;
            return "("+ getRexpForMax(min, "-") + "|[0-9]|[1-9][0-9]*)";
        } else if (min > 0) {
            // no prefix required
            return "(" + getRexpForMin(min, "") + ")";
        } else { // treat zero as a special case
            return "([1-9]|[1-9][0-9]+)";
        }
    }

    public static String getRexpForMax(long max) {
        if(max < 0) {
            max *= -1;
            return "("+ getRexpForMin(max, "-") + ")";
        } else if (max > 0) {
            // no prefix required
            return "(" + getRexpForMax(max, "") + "|0|-[1-9][0-9]*)";
        } else { // treat zero as a special case
            return "(-[1-9]|-[1-9][0-9]+)";
        }
    }

    private static String getRexpForMin(long min, String pfx) {

        // The procedure below just works for non-negative integers
        assert (min > 0);

        String mins = Long.toString(min);
        char[] minc = mins.toCharArray();

        StringBuilder drexp = new StringBuilder();
        String option = "";

        for (int l = minc.length - 1; l >= 0; l--) {

            String carry = "";

            if (drexp.length() > 0) {
                option = "|";
            }


            char digit = minc[l];

            if (digit != '9') {

                if (l != minc.length) {
                    ++digit;

                    if (digit != '9') {
                        carry = "[" + (digit) + "-9]";
                    } else {
                        carry = "9";
                    }

                    drexp.insert(0,pfx+
                            mins.substring(0, l) +
                                    carry + StringUtils.repeat("[0-9]", mins.length() - l - 1) + option);
                }
            }
        }

        // Meta rule for matching everything that has more digits
        if(drexp.length() > 0) {
            drexp.append("|");
        }
        drexp.append(pfx + "[1-9][0-9]{" + (mins.length()) + ",}");



        return drexp.toString();

    }


    public static String getRexpForMax(long max, String pfx) {

        // The procedure below just works for non-negative integers
        assert (max > 0);

        String maxs = Long.toString(max);

        char[] maxc = maxs.toCharArray();

        StringBuilder drexp = new StringBuilder();

        String option = "";

        for (int l = maxc.length - 1; l >= 0; l--) {

            String carry = "";

            if (drexp.length() > 0) {
                option = "|";
            }

            char digit = maxc[l];


            if (digit != '0') {
                if (l != maxc.length) {
                    --digit;

                    if (digit != '0') {
                        carry = "[0-" + (digit) + "]";
                        //LOGGER.info("CARRY " + carry);
                    } else {
                        // if we reached the max significant digit
                        carry = "0";
                    }

                    String digits = maxs.substring(0, l);

                    drexp.insert(0,pfx +
                            digits +
                                    carry + StringUtils.repeat("[0-9]", maxs.length() - l - 1) + option);
                }
            }


            if (maxs.length() > 1) {

                if(drexp.length() > 0) {
                    drexp.append("|");
                }

                drexp.append(pfx +"[1-9][0-9]{0," + (maxs.length() - 2) + "}");
            }

        }
        //LOGGER.info("DREXP " + drexp.toString());
        return drexp.toString();

    }


}

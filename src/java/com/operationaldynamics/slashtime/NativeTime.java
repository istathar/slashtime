/*
 * NativeTime.java
 * 
 * Copyright (c) 2006 Operational Dynamics
 * See LICENCE file for usage and redistribution terms
 */
package com.operationaldynamics.slashtime;

/**
 * Wrapper around strftime. Emulates the use case of SimpleDateFormat: set the
 * format in the constructor, and then format it for a given Date object.
 * 
 * @author Andrew Cowie
 */
class NativeTime
{
    /**
     * Adjust the timezone being used for formatted time/date output
     * calculations.
     */
    /*
     * We don't make an assumption about what state the underlying native
     * environment is. For the cost of potentially having to make duplicate
     * trips, set the environment on order.
     */
    public void setTimeZone(String zoneinfo) {
        if (zoneinfo == null) {
            throw new NullArgumentException();
        }
        if (zoneinfo.equals("")) {
            throw new IllegalStateException("For some reason, this zone's ID is blank");
        }

        tzset(zoneinfo);
    }

    /**
     * Output the date per the format descriptor given in the constructor.
     * 
     * @param format
     *            see strftime(3)
     * @param when
     *            the number of seconds since Epoch being the date/time group
     *            you wish to present according to format
     */
    public String format(String format, long when) {
        if (format == null) {
            throw new NullArgumentException();
        }
        if (format.equals("")) {
            throw new IllegalArgumentException(
                "Not really much point in a format String with nothing in it");
        }
        if (when == 0) {
            throw new IllegalArgumentException("How did you get a time value of 0?");
        }

        return strftime(format, when);
    }

    /**
     * Work out the offset associated with this Place at a given time.
     * 
     * @param when
     *            the Date for which you're needing the offset. This matters
     *            because it has to figure out whether or not it's in DST.
     * @return the number of <b>half</b> hours by which this timezone is offset
     *         from UTC
     */
    public int calculateOffset(long when) {
        String rfc822 = format("%z", when);

        // stupidity: parseInt doesn't understand + but it does understand -
        if (rfc822.charAt(0) == '+') {
            rfc822 = rfc822.substring(1);
        }
        int raw = Integer.parseInt(rfc822);

        int hours = raw / 100;
        int halves = hours * 2;
        if ((raw % 100) != 0) {
            if (raw > 0) {
                halves++;
            } else if (raw < 0) {
                halves--;
            }
        }
        return halves;
    }

    /**
     * Compose a timestamp (the number of seconds since epoch) from individual
     * components of a date/time group. Takes into account the current system
     * timezone setting. We don't worry about seconds.
     * 
     * @param year
     *            the year, four digits
     * @param month
     *            the month, range 1-12
     * @param day
     *            the day, range 1-{28,29,30,31}
     * @param hour
     *            the hour, range 0-23
     * @param minute
     *            the minute, 0-59
     * @return
     */
    public long makeTick(int year, int month, int day, int hour, int minute) {
        if (year < 0) {
            throw new IllegalArgumentException("Year");
        }
        if ((month < 1) || (month > 12)) {
            throw new IllegalArgumentException("Month");
        }
        if ((day < 1) || (day > 31)) {
            throw new IllegalArgumentException("Day");
        }
        if ((hour < 0) || (hour > 23)) {
            throw new IllegalArgumentException("Hour");
        }
        if ((minute < 0) || (minute > 59)) {
            throw new IllegalArgumentException("Minute");
        }

        return mktime(year, month, day, hour, minute, 0);
    }

    private static native void tzset(String zoneinfo);

    private static native String strftime(String format, long timestamp);

    private static native long mktime(int year, int month, int day, int hour, int minute, int second);

    static {
        System.loadLibrary("slashtimejni-0");
    }
}

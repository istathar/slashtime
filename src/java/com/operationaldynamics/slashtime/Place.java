/*
 * Place.java
 * 
 * Copyright (c) 2006-2008 Operational Dynamics Consulting Pty Ltd, and Others
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package com.operationaldynamics.slashtime;

import java.io.File;

/**
 * One of the geographical places for which you want to display information.
 * Places are obviously fully described by their zoneinfo style time zone
 * string, but quite often we want to refer to them by a more convenient label
 * such as a specific city (perhaps "Stuttgart" instead of "Europe/Berlin")
 * 
 * @author Andrew Cowie
 */
public class Place
{
    private String zoneName;

    private String city;

    private String country;

    private static String defaultZoneName;

    // halves
    private int startCivilDay = 15;

    private int startWorkDay = 18;

    private int endWorkDay = 34;

    private int endCivilDay = 46;

    static {
        defaultZoneName = TimeZoneHelper.getUserTimeZone();
    }

    public Place(String zonename, String city, String country) {
        setZoneName(zonename);
        setCity(city);
        setCountry(country);
    }

    public String getCity() {
        return city;
    }

    /**
     * Set the common name (typically the name of a city) that will be
     * displayed for this place.
     */
    public void setCity(final String name) {
        if (name == null) {
            throw new NullArgumentException();
        }
        this.city = name;
    }

    /**
     * Get the identifier correspnding to the name of the timezone file in the
     * zoneinfo directory.
     */
    public String getZoneName() {
        return zoneName;
    }

    /**
     * Set the timezone associated with this Place. The name will be checked
     * against the system's zoneinfo directory.
     * 
     * @param zonename
     *            a String specifying the name of the timezone, for example
     *            <code>America/Toronto</code>, <code>Europe/Paris</code>
     *            or <code>UTC</code>.
     */
    public void setZoneName(String zonename) {
        if (zonename == null) {
            throw new NullArgumentException();
        }
        if (zonename.equals("")) {
            throw new IllegalArgumentException();
        }

        String tzfile = TimeZoneHelper.TZDIR + "/" + zonename;

        if (new File(tzfile).exists()) {
            this.zoneName = zonename;
        } else {
            throw new IllegalArgumentException("Timezone data " + tzfile + " not found");
        }
    }

    public String getCountry() {
        return country;
    }

    /**
     * The country or region that this place is found in. Will be used as
     * subtext decorating the city name.
     * 
     * @param country
     *            the name of the country that this place is in. Can be "" if
     *            you wish to ignore this feature.
     */
    public void setCountry(String country) {
        if (country == null) {
            throw new NullArgumentException();
        }
        this.country = country;
    }

    /**
     * @return true if this Place happens to be the one corresponding to Zulu /
     *         GMT / UTC
     */
    public boolean isZulu() {
        /*
         * Explicity testing against the name UTC is ok assuming we explicitly
         * add UTC in the program somewhere (and, better yet, prevent its
         * removal)
         */
        if (zoneName.equals("UTC")) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @return true if this Place matches localtime (default, /etc/localtime,
     *         etc)
     */
    public boolean isLocal() {
        if (zoneName.equals(defaultZoneName)) {
            return true;
        } else {
            return false;
        }
    }

    private void validateHalves(int halves) {
        if ((halves < 0) || (halves > 47)) {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Get the number of half hours (from midnight) that the work day in this
     * Place starts.
     */
    public int getStartWorkDay() {
        return startWorkDay;
    }

    /**
     * Set the number of half hours (from midnight) that the work day in this
     * Place starts.
     * 
     * @param start
     *            not negative and not greater than 47 (ie, 23:30).
     */
    public void setStartWorkDay(int start) {
        validateHalves(start);
        this.startWorkDay = start;
    }

    /**
     * Get the number of half hours since midnight that the work day in this
     * Place ends.
     */
    public int getEndWorkDay() {
        return endWorkDay;
    }

    /**
     * Set the number of half hours (from midnight) that the work day in this
     * Place ends.
     * 
     * @param end
     *            not negative and not greater than 47 (ie, 23:30).
     */
    public void setEndWorkDay(int end) {
        validateHalves(end);
        this.endWorkDay = end;
    }

    /**
     * Get the number of half hours (from midnight) that the civilized day in
     * this Place starts.
     */
    public int getStartCivilDay() {
        return startCivilDay;
    }

    /**
     * Set the number of half hours (from midnight) that the work day in this
     * Place starts.
     * 
     * @param start
     *            not negative and not greater than 47 (ie, 23:30).
     */
    public void setStartCivilDay(int start) {
        validateHalves(start);
        this.startCivilDay = start;
    }

    /**
     * Get the number of half hours since midnight that the civil day in this
     * Place ends.
     */
    public int getEndCivilDay() {
        return endCivilDay;
    }

    /**
     * Set the number of half hours (from midnight) that the civil day in this
     * Place ends.
     * 
     * @param end
     *            not negative and not greater than 47 (ie, 23:30).
     */
    public void setEndCivilDay(int end) {
        validateHalves(end);
        this.endCivilDay = end;
    }

    /**
     * @param sinceMidnight
     *            the number of halves since midnight
     * @return whether or not it is working hours at this Place
     */
    public boolean isWorkHours(int sinceMidnight) {
        if (startWorkDay == endWorkDay) {
            return false;
        }

        if (endWorkDay > startWorkDay) {
            if ((sinceMidnight >= startWorkDay) && (sinceMidnight < endWorkDay)) {
                return true;
            } else {
                return false;
            }
        } else {
            // inverse relationship, for whatever reason crossing midnight.
            if ((sinceMidnight < endWorkDay) || (sinceMidnight >= startWorkDay)) {
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * @param sinceMidnight
     *            the number of halves since midnight
     * @return whether or not it is working hours at this Place
     */
    public boolean isCivilHours(int sinceMidnight) {
        if (startCivilDay == endCivilDay) {
            return false;
        }

        if (endCivilDay > startCivilDay) {
            if ((sinceMidnight >= startCivilDay) && (sinceMidnight < endCivilDay)) {
                return true;
            } else {
                return false;
            }
        } else {
            // inverse relationship, for whatever reason crossing midnight.
            if ((sinceMidnight < endCivilDay) || (sinceMidnight >= startCivilDay)) {
                return true;
            } else {
                return false;
            }
        }
    }

    public String toString() {
        return city;
    }
}

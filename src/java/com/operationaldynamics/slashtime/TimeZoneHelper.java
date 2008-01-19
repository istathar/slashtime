/*
 * TimeZoneHelper.java
 * 
 * Copyright (c) 2006-2008 Operational Dynamics Consulting Pty Ltd, and Others
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package com.operationaldynamics.slashtime;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

/**
 * Utilities to determine the current user's timezone.
 * 
 * @author Andrew Cowie
 * @author Benjamin P. Jung
 */
class TimeZoneHelper
{
    /**
     * The parent directory where the tzfile zoneinfo time zone information
     * files are stored. TZDIR is the name of the environment variable that
     * glibc supposedly checks; we could reach through to native to get it I
     * suppose.
     */
    static final String TZDIR = "/usr/share/zoneinfo";

    /**
     * Attempt to determine the timezone of the current user. This progresses
     * through a series of fallbacks:
     * 
     * <ul>
     * <li>Java property "user.timezone", if set
     * <li>Environment variable "TZ", if it exists
     * <li>Contents of file /etc/timezone, if it exists
     * <li>What zoneinfo file /etc/localtime is pointing at, if it is indeed
     * a symlink into /usr/share/zoneinfo
     * </ul>
     * 
     * @return an empty String if we were not able to find out the user's
     *         timezone, a String representating the timezone otherwise
     *         suitable for creating a Place.
     */
    static String getUserTimeZone() {

        /*
         * 1. Check system property "user.timezone"
         */
        final String userTimezone = System.getProperty("user.timezone");
        if ((userTimezone != null) && !userTimezone.equals("")) {
            return userTimezone;
        }

        /*
         * 2. Check for $TZ
         */
        final String tzEnv = System.getenv("TZ");
        if ((tzEnv != null) && !tzEnv.equals("")) {
            return tzEnv;
        }

        /*
         * 3. Check /etc/timezone
         */
        try {
            final String osName = System.getProperty("os.name");
            final File timezoneFile;
            if (osName.equalsIgnoreCase("linux")) {
                timezoneFile = new File("/etc/timezone");
            } else if (osName.equalsIgnoreCase("solaris")) {
                timezoneFile = new File("/etc/TIMEZONE");
            } else {
                timezoneFile = null;
            }
            if ((timezoneFile != null) && timezoneFile.exists() && timezoneFile.isFile()) {
                final BufferedReader in;
                in = new BufferedReader(new FileReader(timezoneFile));
                final String timezone = in.readLine();
                if ((timezone != null) && !timezone.equals("")) {
                    return timezone;
                }
            }
        } catch (final Exception e) {
            // ignore, unless you're debugging
        }

        /*
         * 4. Check /etc/localtime
         */
        try {
            final String localtime = "/etc/localtime";
            final File localtimeFile = new File(localtime);
            if (localtimeFile.exists()) {
                final String canonical = localtimeFile.getCanonicalPath();
                /*
                 * If it's not a symlink, then the canonical path and the
                 * original source path will be the same, we can't get
                 * anywhere further with this path.
                 */
                if (!canonical.equals(localtimeFile)) {
                    /*
                     * File's getCanonicalPath() returns the absolute and
                     * resolved filename of a symlink, so from /etc/localtime
                     * we get /usr/share/zoneinfo/Australia/Sydney from which
                     * we can simply extract the local zone name.
                     */
                    return canonical.substring(TZDIR.length() + 1);
                }
            }
        } catch (final Exception e) {
            // ignore, although we shouldn't expect any.
        }

        /*
         * We have failed! Alas. Return an empty string, which is the marker
         * used later to skip highlighting the home location.
         */
        return "";
    }
}

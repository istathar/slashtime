/*
 * Loader.java
 *
 * Copyright (c) 2008 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the library it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package slashtime.services;

import static java.io.StreamTokenizer.TT_EOF;
import static java.io.StreamTokenizer.TT_EOL;
import static java.io.StreamTokenizer.TT_WORD;
import static java.lang.System.getProperty;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.StreamTokenizer;
import java.util.ArrayList;

import slashtime.domain.Place;

/**
 * Load the list of Places that we will be showing in the ZonesWindow.
 * 
 * @author Andrew Cowie
 */
/*
 * This could evolve to being a proper OO class with state 'n such now that
 * it's in a stand alone file. That said, the parser is pretty simple, and
 * there's not much state.
 */
public final class Loader
{
    /**
     * Parse the user's timezone list. Fall back to hard-coded built-in data
     * if the file isn't present. Returns an array of Places
     */
    public static Place[] loadPlaceList() {
        final File file;

        file = new File(getProperty("user.home") + "/.tzlist");

        if (file.exists()) {
            return loadUserZoneList(file);
        } else {
            return loadFallbackData();
        }
    }

    /**
     * Attempt to parse ~/.tzlist for Place data. The file format is
     * 
     * "zonename" "City" "Country"
     * 
     * with one Place expected per line. Lines starting with # are ignored.
     */
    /*
     * We use a StreamTokenizer to do the heavy lifting; we wrap it around a
     * LineNumberReader so we can get back to the start of a line and re-read
     * it if necessary to write it to stderr for diagnostic purposes.
     */
    private static Place[] loadUserZoneList(File tzlist) {
        final LineNumberReader line;
        final StreamTokenizer parser;
        final ArrayList<Place> places;
        String zone, city, country;
        Place place;
        final Place[] result;

        places = new ArrayList<Place>(25);

        place = new Place("UTC", "Zulu", "Universal Time");
        places.add(place);

        try {
            line = new LineNumberReader(new FileReader(tzlist));
            line.mark(128);

            parser = new StreamTokenizer(line);
            parser.commentChar('#');
            parser.quoteChar('"');
            parser.eolIsSignificant(true);

            zone = null;
            city = null;
            country = null;

            while (parser.nextToken() != TT_EOF) {
                if (parser.ttype == TT_EOL) {
                    if (!((zone == null) && (city == null) && (country == null))) {
                        System.err.println("Warning: premature EOL, line " + parser.lineno() + ":");
                        line.reset();
                        System.err.println(line.readLine());
                    }

                    zone = null;
                    city = null;
                    country = null;

                    line.mark(128);
                    continue;
                }

                if (!((parser.ttype == TT_WORD) || (parser.ttype == '"'))) {
                    continue;
                }

                if (zone == null) {
                    zone = parser.sval;
                } else if (city == null) {
                    city = parser.sval;
                } else if (country == null) {
                    country = parser.sval;

                    place = new Place(zone, city, country);
                    places.add(place);

                    zone = null;
                    city = null;
                    country = null;

                    continue;
                }
            }

            line.close();
        } catch (FileNotFoundException fnfe) {
            // surely not? We already checked its existence
            throw new IllegalStateException(fnfe);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        result = new Place[places.size()];
        return places.toArray(result);
    }

    /**
     * Hard coded default data to use in the event the user lacks a .tzlist
     * file. UTC first, as required elsewhere.
     */
    private static Place[] loadFallbackData() {
        System.err.println("Warning: ~/.tzlist not found. Using fallback Place list instead.");

        return new Place[] {
                new Place("UTC", "Zulu", "Universal Time"),
                new Place("America/Montreal", "Toronto", "Canada"),
                new Place("America/Vancouver", "Vancouver", "Canada"),
                new Place("Australia/Sydney", "Sydney", "Australia"),
                new Place("Europe/Paris", "Paris", "France"),
                new Place("America/Halifax", "Halifax", "Canada"),
                new Place("Europe/London", "London", "UK"),
                new Place("Asia/Calcutta", "Bangalore", "India"),
                new Place("Asia/Hong_Kong", "Hong Kong", "China"),
                new Place("Pacific/Auckland", "Auckland", "New Zealand"),
                new Place("Pacific/Honolulu", "Hawaii", "USA"),
                new Place("America/Los_Angeles", "Los Angeles", "USA"),
                new Place("America/New_York", "New York", "USA"),
                new Place("America/Edmonton", "Calgary", "Canada"),
                new Place("Australia/Adelaide", "Adelaide", "Australia"),
                new Place("Asia/Tokyo", "Tokyo", "Japan"),
                new Place("Asia/Singapore", "Singapore", "Singapore"),
                new Place("Asia/Dubai", "Dubai", "UAE"),
                new Place("Australia/Perth", "Perth", "Australia"),
                new Place("Europe/Moscow", "Moscow", "Russia"),
        };
    }

    private Loader() {}
}

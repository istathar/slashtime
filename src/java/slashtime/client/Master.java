/*
 * Master.java
 * 
 * Copyright (c) 2006-2008 Operational Dynamics Consulting Pty Ltd
 *
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package slashtime.client;

import org.freedesktop.bindings.Internationalization;
import org.gnome.glib.Glib;
import org.gnome.gtk.Gtk;

import slashtime.domain.Place;
import slashtime.ui.UserInterface;

/**
 * Main entry point for slashtime program.
 * 
 * @author Andrew Cowie
 */
public final class Master
{
    /**
     * Global re-entry point for code in other layers to be able to request
     * actions of the user interface.
     */
    public static UserInterface ui = null;

    public static void main(String[] args) {
        Glib.setProgramName("slashtime");
        Gtk.init(args);
        Internationalization.init("slashtime", "share/locale/");

        /*
         * If you specify a zone name on the command line it will become a
         * third icon in the ZonesWindow denoting where "home" is (as opposed
         * to where you are now). This is a bit of a hack at the moment, but
         * useful when travelling. It will likely be replaced with a proper
         * setting once we have a GConf binding.
         */

        if (args.length == 1) {
            specifyHome(args[0]);
        }

        /*
         * Now, on to business. Build the GUI and attach it to the global
         * re-entry point
         */

        ui = new UserInterface();

        /*
         * And, fire the main event loop.
         */

        Gtk.main();
    }

    private static void specifyHome(String zonename) {
        Place.setHomeZoneName(zonename);
    }
}

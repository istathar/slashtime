/*
 * Master.java
 * 
 * Copyright (c) 2006-2007 Operational Dynamics Consulting Pty Ltd
 *
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package com.operationaldynamics.slashtime;

import org.gnome.gdk.Pixbuf;
import org.gnome.gtk.Gtk;

/**
 * Main entry point for slashtime program.
 * 
 * @author Andrew Cowie
 */
public final class Master
{
    static ZonesWindow zones;

    static MeetingWindow meeting;

    static DockedIndicator status;

    /**
     * The application icon.
     */
    /*
     * I might prefer this to be wrapped by a UserInterface class so it could
     * be encapsulated a bit better and reached via Master.ui.icon. It's less
     * of an issue with static imports.
     */
    static Pixbuf marble;

    static Pixbuf calendar;

    public static void main(String[] args) {
        Gtk.init(args);

        zones = new ZonesWindow();
        meeting = null;
        status = new DockedIndicator();

        Gtk.main();
    }
}

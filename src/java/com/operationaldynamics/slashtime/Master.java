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
package com.operationaldynamics.slashtime;

import java.io.FileNotFoundException;

import org.gnome.gdk.Pixbuf;
import org.gnome.gtk.Gtk;

/**
 * Main entry point for slashtime program.
 * 
 * @author Andrew Cowie
 */
public final class Master
{
    public static void main(String[] args) {
        Gtk.init(args);

        loadImages();

        ui.zones = new ZonesWindow();
        ui.meeting = null;
        ui.status = new DockedIndicator();

        Gtk.main();
    }

    private static void loadImages() {
        try {
            images.marble = new Pixbuf("share/pixmaps/slashtime-marble.png");
            images.gmt = new Pixbuf("share/pixmaps/slashtime-marble.png", 22, 22, true);
            images.home = new Pixbuf("share/pixmaps/slashtime-home.png", 24, 24, true);
            images.local = new Pixbuf("share/pixmaps/slashtime-local.png", 24, 24, true);
            images.calendar = new Pixbuf("share/pixmaps/slashtime-meeting.png", 20, 20, true);
        } catch (FileNotFoundException fnfe) {
            System.err.println("Icon file not found: " + fnfe.getMessage());
        }
    }
}

/**
 * Globally accessible references to images that are re-used in various
 * contexts.
 */
/*
 * This is an attmept to encapsulated global variables a bit better. This was
 * originally inspired by ObjectiveAccount's ProcedureCluent.ui.blah() where
 * ui was of type UserInferface and had various application specific global
 * actions on it. It's less of an issue with static imports, but still it's a
 * pain to namespace these bloody things. Yes, yes, it's horribly bad form to
 * name a class with a lower case letter. But the completions look excellent.
 */
class images
{
    /**
     * The application icon.
     */
    static Pixbuf marble;

    /**
     * The MeetingWindow icon.
     */
    static Pixbuf calendar;

    /**
     * Where GMT is. Scaled to fit in the ZonesWindow.
     */
    static Pixbuf gmt;

    /**
     * Where your current timezone is. Scaled to fit in the ZonesWindow.
     */
    static Pixbuf home;

    /**
     * Your current location. Scaled to fit in the ZonesWindow.
     */
    static Pixbuf local;
}

class ui
{
    static ZonesWindow zones;

    static MeetingWindow meeting;

    static DockedIndicator status;
}

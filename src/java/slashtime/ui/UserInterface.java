/*
 * UserInterface.java
 *
 * Copyright (c) 2008-2010 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by its authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package slashtime.ui;

import java.io.FileNotFoundException;

import org.gnome.gdk.Pixbuf;
import org.gnome.gtk.AboutDialog;
import org.gnome.gtk.Gtk;
import org.gnome.gtk.Window;

import slashtime.client.Version;

import static org.freedesktop.bindings.Internationalization._;

/**
 * Harness for the UI code.
 * 
 * @author Andrew Cowie
 */
/*
 * At present this class has nothing exciting (ie, public) besides the
 * constructor but the GUI layer gets a fair bit done by making calls on the
 * instances of the various windows, made available through fields of this
 * class.
 */
public class UserInterface
{
    ZonesWindow zones;

    MeetingWindow meeting;

    DockedIndicator status;

    /**
     * Constructing this will build the UI elements representing the program.
     * While not enforced as a Singleton (no need to) it is only expected that
     * this would be constructed once, by the client layer, and assigned to
     * Master.ui
     */
    public UserInterface(boolean show) {
        loadImages();
        setupApplication();
        setupWindows(show);
    }

    private void setupWindows(boolean show) {
        meeting = null;
        zones = new ZonesWindow(show);
        status = new DockedIndicator();
    }

    private void loadImages() {
        try {
            images.marble = new Pixbuf("share/pixmaps/slashtime-marble.png");
            images.tray = new Pixbuf("share/pixmaps/slashtime-marble.png", 20, 20, true);
            images.gmt = new Pixbuf("share/pixmaps/slashtime-marble.png", 22, 22, true);
            images.home = new Pixbuf("share/pixmaps/slashtime-home.png", 24, 24, true);
            images.local = new Pixbuf("share/pixmaps/slashtime-local.png", 24, 24, true);
            images.calendar = new Pixbuf("share/pixmaps/slashtime-meeting.png", 20, 20, true);
        } catch (FileNotFoundException fnfe) {
            System.err.println("Icon file not found: " + fnfe.getMessage());
        }
    }

    private void setupApplication() {
        Gtk.setDefaultIcon(images.marble);
    }

    public void showAbout() {
        final AboutDialog dialog;

        dialog = new AboutDialog();
        dialog.setAuthors(new String[] {
            "Andrew Cowie <andrew@operationaldynamics.com>",
        });
        dialog.setComments(_("Show the time in various places!"));
        dialog.setCopyright(_("Copyright")
                + " \u00A9 2003-2010 Operational Dynamics Consulting Pty Ltd, " + _("and Others"));
        dialog.setLicense(_("This program is made available to you by its authors under the terms of the GNU General Public Licence, version 2."));
        dialog.setLogo(images.marble);
        dialog.setTranslatorCredits(_("translator-credits"));
        dialog.setVersion(Version.VERSION);
        dialog.setWebsite("http://research.operationaldynamics.com/projects/slashtime/");
        dialog.setWebsiteLabel(_("Website"));
        dialog.setWrapLicense(true);

        dialog.run();

        dialog.hide();
    }

    /**
     * Tear down the user interface (and terminate the application).
     */
    public void shutdown() {
        Window w;

        if (meeting != null) {
            w = meeting.getWindow();
            w.hide();
        }
        if (zones != null) {
            w = zones.getWindow();
            w.hide();
        }

        Gtk.mainQuit();
    }

}

/**
 * Package accessible references to images that are re-used in various
 * contexts.
 */
/*
 * Yes, yes, it's horribly bad form to name a class with a lower case letter.
 * But the completions look excellent, and it's all only package visible.
 */
class images
{
    /**
     * The application icon.
     */
    static Pixbuf marble;

    /**
     * The application icon, small for the notification area.
     */
    static Pixbuf tray;

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

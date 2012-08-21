/*
 * Slashtime, a small program which displays the time in various places.
 *
 * Copyright © 2008-2011 Operational Dynamics Consulting, Pty Ltd
 *
 * The code in this file, and the program it is a part of, is made available
 * to you by its authors as open source software: you can redistribute it
 * and/or modify it under the terms of the GNU General Public License version
 * 2 ("GPL") as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GPL for more details.
 *
 * You should have received a copy of the GPL along with this program. If not,
 * see http://www.gnu.org/licenses/. The authors of this program may be
 * contacted through http://research.operationaldynamics.com/projects/slashtime/.
 */
package slashtime.ui;

import java.io.FileNotFoundException;

import org.gnome.gdk.InterpType;
import org.gnome.gdk.Pixbuf;
import org.gnome.gtk.AboutDialog;
import org.gnome.gtk.Gtk;
import org.gnome.gtk.Window;

import slashtime.client.Version;

import static org.freedesktop.bindings.Internationalization._;
import static slashtime.client.Master.app;

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

    /**
     * Constructing this will build the UI elements representing the program.
     * While not enforced as a Singleton (no need to) it is only expected that
     * this would be constructed once, by the client layer, and assigned to
     * Master.ui
     */
    public UserInterface() {
        loadImages();
        setupApplication();
        setupWindows();
    }

    private void setupWindows() {
        meeting = null;
        zones = new ZonesWindow();
    }

    private void loadImages() {
        try {
            images.marble = new Pixbuf("share/icons/hicolor/48x48/apps/slashtime.png");
            images.gmt = images.marble.scale(22, 22, InterpType.BILINEAR);
            images.home = new Pixbuf("share/slashtime/images/home.png", 24, 24, true);
            images.local = new Pixbuf("share/slashtime/images/local.png", 24, 24, true);
            images.calendar = new Pixbuf("share/slashtime/images/meeting.png", 20, 20, true);
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
                + " \u00A9 2003-2011 Operational Dynamics Consulting, Pty Ltd " + _("and Others"));
        dialog.setLicense(_("licence-text"));
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
            app.removeWindow(w);
        }
        app.quit();
    }

    public void display() {
        zones.present();
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

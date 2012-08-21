/*
 * Slashtime, a small program which displays the time in various places.
 *
 * Copyright © 2006-2011 Operational Dynamics Consulting, Pty Ltd
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
package slashtime.client;

import org.freedesktop.bindings.Internationalization;
import org.gnome.glib.ApplicationCommandLine;
import org.gnome.glib.ApplicationFlags;
import org.gnome.glib.Glib;
import org.gnome.gtk.Application;
import org.gnome.gtk.Gtk;

import slashtime.domain.Place;
import slashtime.ui.UserInterface;

import static java.lang.System.exit;

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
    public static UserInterface ui;

    public static Application app;

    public static void main(String[] args) {
        final int status;

        Glib.setProgramName("slashtime");
        Gtk.init(args);
        Internationalization.init("slashtime", "share/locale/");

        app = new Application("com.operationaldynamics.Slashtime", ApplicationFlags.HANDLES_COMMAND_LINE);

        /*
         * Now, on to business. Build the GUI and attach it to the global
         * re-entry point
         */

        app.connect(new Application.Startup() {
            public void onStartup(Application source) {
                ui = new UserInterface();
            }
        });

        app.connect(new Application.Activate() {
            public void onActivate(Application source) {
                ui.display();
            }
        });

        /*
         * If you specify a zone name on the command line it will become a
         * third icon in the ZonesWindow denoting where "home" is (as opposed
         * to where you are now). This is a bit of a hack at the moment, but
         * useful when travelling. It will likely be replaced with a proper
         * setting once we have a GConf binding.
         */

        app.connect(new Application.CommandLine() {
            public int onCommandLine(Application source, ApplicationCommandLine remote) {
                final String[] args;

                args = remote.getArguments();

                if (args.length > 1) {
                    specifyHome(args[1]);
                }

                /*
                 * Trigger the Application.Activate signal, where the
                 * ZonesWindow is raised to front.
                 */

                app.activate();

                /*
                 * Indicate that the remote instance that sent us the command
                 * line arguments should terminate as soon as possible.
                 */

                remote.exit();

                return 0;
            }
        });

        /*
         * And, fire the main event loop. This blocks.
         */

        status = app.run(args);

        exit(status);
    }

    private static void specifyHome(String zonename) {
        Place.setHomeZoneName(zonename);
    }

    public static final boolean DEBUG = false;
}

/*
 * DockedIndicator.java
 *
 * Copyright (c) 2008-2010 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package slashtime.ui;

import org.gnome.gtk.ImageMenuItem;
import org.gnome.gtk.Menu;
import org.gnome.gtk.MenuItem;
import org.gnome.gtk.StatusIcon;
import org.gnome.gtk.Stock;

import static slashtime.client.Master.ui;

/**
 * Wrapper around a StatusIcon in the notification area. This is somewhat a
 * temporary measure until we have a GNOME applet binding available, but in
 * the mean time gives the user a way (by clicking) to raise the ZonesWindow.
 * 
 * @author Andrew Cowie
 */
class DockedIndicator
{
    private StatusIcon si;

    private Menu menu = null;

    DockedIndicator() {
        si = new StatusIcon(images.tray);

        si.connect(new StatusIcon.Activate() {
            public void onActivate(StatusIcon source) {
                ui.zones.toggle();
            }
        });

        /*
         * Establish the menu that pops up on when the StatusIcon is right
         * clicked.
         */

        si.connect(new StatusIcon.PopupMenu() {
            public void onPopupMenu(StatusIcon source, int button, int activateTime) {
                menu.popup(source);
            }
        });

        menu = new Menu();
        menu.append(new ImageMenuItem(Stock.ABOUT, new MenuItem.Activate() {
            public void onActivate(MenuItem source) {
                ui.showAbout();
            }
        }));
        menu.append(new ImageMenuItem(Stock.QUIT, new MenuItem.Activate() {
            public void onActivate(MenuItem source) {
                ui.shutdown();
            }
        }));
        menu.showAll();
    }
}

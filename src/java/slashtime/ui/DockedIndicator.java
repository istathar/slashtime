/*
 * Slashtime, a small program which displays the time in various places.
 *
 * Copyright © 2008-2010 Operational Dynamics Consulting, Pty Ltd
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

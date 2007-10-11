/*
 * DockedIndicator.java
 *
 * Copyright (c) 2007 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the library it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package com.operationaldynamics.slashtime;

import org.gnome.gtk.StatusIcon;
import static com.operationaldynamics.slashtime.Master.marble;

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

    DockedIndicator() {
        si = new StatusIcon(marble);

        si.connect(new StatusIcon.ACTIVATE() {
            public void onActivate(StatusIcon source) {
                Master.zones.updateNow();
            }
        });
    }
}

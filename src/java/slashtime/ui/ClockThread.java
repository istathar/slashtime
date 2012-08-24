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

import org.gnome.glib.Glib;
import org.gnome.glib.Handler;

import static java.lang.System.currentTimeMillis;
import static slashtime.client.Master.ui;

/**
 * Wrapper around a Thread to run the update loop that wakes up every 60
 * seconds on the 0 when running, and which simply waits when not running.
 * 
 * @author Andrew Cowie
 */
class ClockThread
{
    private final Thread timer;

    private boolean running;

    ClockThread() {
        running = true;

        timer = new Thread() {
            public void run() {
                // ms
                long time, delay;

                while (true) {
                    try {
                        if (running) {
                            time = currentTimeMillis();
                            delay = 60000 - time % 60000;

                            sleep(delay);

                            if (ui.meeting != null) {
                                continue;
                            }

                            Glib.idleAdd(new Handler() {
                                public boolean run() {
                                    ui.zones.updateNow();
                                    return false;
                                }
                            });
                        } else {
                            synchronized (timer) {
                                wait();
                            }
                        }
                    } catch (InterruptedException ie) {
                        /*
                         * It sure would be nice if interrupt() actually did
                         * happen as a result of the process being paused [by
                         * the shell, suspend, hibernate] and then resumed.
                         * So, TODO we'll need some hacky logic to deal with
                         * that. Some other Thread to watch a /sys file?
                         * Listen for a DBus message? Either way, that thread
                         * can then interrupt() this one.
                         */
                    }
                }
            }
        };

        timer.setDaemon(true);
        timer.start();
    }

    /**
     * Tell the ClockThread whether to be running or not. If the state is
     * changed to running then it will update the display.
     */
    void setRunning(boolean setting) {
        if (setting == running) {
            return;
        }

        if (!running) {
            ui.zones.updateNow();
        }

        running = setting;

        synchronized (timer) {
            timer.interrupt();
        }
    }
}

/*
 * ClockThread.java
 *
 * Copyright (c) 2008 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by its authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package slashtime.ui;

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
        running = false;

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

                            ui.zones.updateNow();
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

    void setRunning(boolean setting) {
        if (setting != running) {
            running = setting;

            synchronized (timer) {
                timer.interrupt();
            }
        }
    }

}

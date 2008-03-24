/*
 * ZonesWindow.java
 * 
 * Copyright (c) 2006-2008 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package com.operationaldynamics.slashtime;

import static com.operationaldynamics.slashtime.Loader.loadPlaceList;
import static org.freedesktop.bindings.Time.formatTime;
import static org.freedesktop.bindings.Time.setTimeZone;
import static org.gnome.gtk.Alignment.CENTER;
import static org.gnome.gtk.Alignment.LEFT;
import static org.gnome.gtk.Alignment.TOP;

import org.gnome.gdk.Color;
import org.gnome.gdk.Event;
import org.gnome.gdk.EventButton;
import org.gnome.gdk.EventFocus;
import org.gnome.gdk.EventVisibility;
import org.gnome.gdk.MouseButton;
import org.gnome.gdk.VisibilityState;
import org.gnome.gtk.AboutDialog;
import org.gnome.gtk.Action;
import org.gnome.gtk.Alignment;
import org.gnome.gtk.CellRendererPixbuf;
import org.gnome.gtk.CellRendererText;
import org.gnome.gtk.DataColumn;
import org.gnome.gtk.DataColumnInteger;
import org.gnome.gtk.DataColumnPixbuf;
import org.gnome.gtk.DataColumnReference;
import org.gnome.gtk.DataColumnString;
import org.gnome.gtk.Gtk;
import org.gnome.gtk.Image;
import org.gnome.gtk.ImageMenuItem;
import org.gnome.gtk.ListStore;
import org.gnome.gtk.Menu;
import org.gnome.gtk.SortType;
import org.gnome.gtk.StateType;
import org.gnome.gtk.Stock;
import org.gnome.gtk.TreeIter;
import org.gnome.gtk.TreeModelSort;
import org.gnome.gtk.TreePath;
import org.gnome.gtk.TreeSelection;
import org.gnome.gtk.TreeView;
import org.gnome.gtk.TreeViewColumn;
import org.gnome.gtk.VBox;
import org.gnome.gtk.Widget;
import org.gnome.gtk.Window;

/**
 * Display a TreeView (ListView form) with one row per {@link Place}.
 * 
 * @author Andrew Cowie
 */
class ZonesWindow
{
    private final Window window;

    /**
     * Our perception of whether or not the ZonesWindow is currently on screen
     * to front versus minimized or obscured.
     */
    private boolean up = false;

    private final VBox top;

    private final TreeView view;

    private final TreeViewColumn vertical;

    private final ListStore model;

    private final TreeModelSort sorted;

    private final TreeSelection selection;

    private final Menu menu;

    private final DataColumnPixbuf iconImage;

    private final DataColumnString placeMarkup;

    private final DataColumnString timeMarkup;

    private final DataColumnInteger timeSort;

    private final DataColumnString offsetMarkup;

    private final DataColumnInteger offsetSort;

    private final DataColumnString rowColor;

    private final DataColumnString rowBackground;

    private final DataColumnReference placeObject;

    private Place current;

    private Place target;

    /**
     * Build the main GUI window
     */
    ZonesWindow() {
        CellRendererPixbuf image;
        CellRendererText text;
        final int s_w, s_h, w, h;
        final Action popMeeting, closeDown, popAbout;

        window = new Window();

        window.setIcon(images.marble);

        window.setTitle("slashtime");
        window.setDecorated(false);
        window.setBorderWidth(1);
        window.modifyBackground(StateType.NORMAL, Color.BLACK);

        top = new VBox(false, 0);

        iconImage = new DataColumnPixbuf();
        placeMarkup = new DataColumnString();
        timeMarkup = new DataColumnString();
        timeSort = new DataColumnInteger();
        offsetMarkup = new DataColumnString();
        offsetSort = new DataColumnInteger();
        rowColor = new DataColumnString();
        rowBackground = new DataColumnString();
        placeObject = new DataColumnReference();

        model = new ListStore(new DataColumn[] {
                iconImage,
                placeMarkup,
                timeMarkup,
                timeSort,
                offsetMarkup,
                offsetSort,
                rowColor,
                rowBackground,
                placeObject,
        });

        sorted = new TreeModelSort(model);

        view = new TreeView(sorted);
        view.setRulesHint(false);
        view.setHeadersVisible(false);
        view.setEnableSearch(false);

        /*
         * Unusually, we can pack all the CellRenderers into one
         * TreeViewColumn as they reserve a constant width per actual row.
         * This has the nice side effect of eliminating the one pixel boundary
         * between the former TreeViewColumns whose headings we weren't
         * looking at anyway.
         */

        vertical = view.appendColumn();

        /* Icon */
        image = new CellRendererPixbuf(vertical);
        image.setPixbuf(iconImage);
        image.setBackground(rowBackground);

        /* Place */
        text = new CellRendererText(vertical);
        text.setAlignment(LEFT, TOP);
        text.setMarkup(placeMarkup);
        text.setForeground(rowColor);
        text.setBackground(rowBackground);

        /* Date and Time */
        text = new CellRendererText(vertical);
        text.setAlignment(CENTER, Alignment.TOP);
        text.setMarkup(timeMarkup);
        text.setForeground(rowColor);
        text.setBackground(rowBackground);

        /* Offset */
        text = new CellRendererText(vertical);
        text.setAlignment(Alignment.CENTER, Alignment.TOP);
        text.setMarkup(offsetMarkup);
        text.setForeground(rowColor);
        text.setBackground(rowBackground);

        sortByWallTime();

        /*
         * Pack widgets, and prepare Window properties.
         */

        top.packStart(view, true, true, 0);

        window.connect(new Window.DELETE_EVENT() {
            public boolean onDeleteEvent(Widget source, Event event) {
                Gtk.mainQuit();
                return false;
            }
        });

        /*
         * When focus leaves the ZonesWindow, deselect so that it's not left
         * with a blue selected row for no terribly useful reason.
         */

        view.connect(new Widget.FOCUS_OUT_EVENT() {
            public boolean onFocusOutEvent(Widget source, EventFocus event) {
                selection.unselectAll();
                return false;
            }
        });

        view.connect(new TreeView.ROW_ACTIVATED() {
            public void onRowActivated(TreeView source, TreePath path, TreeViewColumn vertical) {
                final TreeIter row;

                row = model.getIter(path);
                current = (Place) model.getValue(row, placeObject);

                if (ui.meeting == null) {
                    updateNow();
                } else {
                    ui.meeting.setPlace(current);
                }
            }
        });

        selection = view.getSelection();

        selection.connect(new TreeSelection.CHANGED() {
            public void onChanged(TreeSelection source) {
                final TreeIter row;

                row = selection.getSelected();

                if (row != null) {
                    target = (Place) sorted.getValue(row, placeObject);
                } else {
                    target = current;
                }

                if (ui.meeting != null) {
                    ui.meeting.setPlace(target);
                }
            }
        });

        /*
         * Deal with setting the up variable so we can react to activation on
         * the DockedIndicator accordingly.
         */

        window.connect(new Widget.VISIBILITY_NOTIFY_EVENT() {
            public boolean onVisibilityNotifyEvent(Widget source, EventVisibility event) {
                final VisibilityState state;

                state = event.getState();

                if ((state == VisibilityState.FULLY_OBSCURED) || (state == VisibilityState.PARTIAL)) {
                    up = false;
                } else {
                    up = true;
                }
                return false;
            }
        });

        window.connect(new Widget.UNMAP_EVENT() {
            public boolean onUnmapEvent(Widget source, Event event) {
                up = false;
                return false;
            }
        });

        window.add(top);

        /*
         * Populate the list and run an initial update of the time and offset
         * readouts. It's necessary to update the time fields here as a
         * preload to ensure the TreeView is properly sized and that all
         * columns are showing.
         */

        populate();
        updateNow();

        /*
         * Position the window and present. FUTURE If this becomes an applet,
         * then the position will have to be south docked onto the panel above
         * the time display.
         */

        window.showAll();
        window.hide();

        s_w = window.getScreen().getWidth();
        s_h = window.getScreen().getHeight();

        w = window.getWidth();
        h = window.getHeight();

        window.move(s_w - w - 20, s_h - h - 30);
        window.present();

        /*
         * Fire up the interrupt timer to update the time readouts.
         */

        final Thread clock;

        clock = new Thread() {
            public void run() {
                // ms
                long time, delay;
                // s
                long tick;

                while (true) {
                    time = System.currentTimeMillis();
                    delay = 60000 - time % 60000;

                    try {
                        Thread.sleep(delay);
                    } catch (InterruptedException ie) {
                        /*
                         * It sure would benice if interrupt() actually did
                         * happen as a result of the process being paused [by
                         * the shell, suspend, hibernate] and then resumed.
                         * So, TODO we'll need some hacky logic to deal with
                         * that. Some other Thread to watch a /sys file?
                         * Listen for a DBus message? Either way, that thread
                         * can then interrupt() this one.
                         */
                    }

                    if (ui.meeting != null) {
                        continue;
                    }

                    time = System.currentTimeMillis();

                    tick = time / 1000;
                    update(tick);
                }
            }
        };
        clock.setDaemon(true);
        clock.start();

        // AccelGroup ag = new AccelGroup();
        // window.addAccelGroup(ag);

        /*
         * Create the action to popup a MeetingWindow
         */
        popMeeting = new Action("meeting", "Meeting");
        popMeeting.setTooltip("Pop up the Meeting planner");
        // popMeeting = new Action("meeting", "Meeting", "Pop up the Meeting
        // planner");
        // AccelMap.changeEntry("<ZonesWindow>/Meeting", Keyval.m,
        // ModifierType.CONTROL_MASK, true);
        //
        // popMeeting.setAccelGroup(ag);
        // popMeeting.setAccelPath("<ZonesWindow>/Meeting");
        // popMeeting.connectAccelerator();
        //
        popMeeting.connect(new Action.ACTIVATE() {
            public void onActivate(Action sourceObject) {
                /*
                 * And the logic to pop up a MeetingWindow. This seems quite
                 * burried, and a bit odd to use Action's activate to fire
                 * this. Perhaps we'll get something out of leaving it here.
                 */
                if (ui.meeting == null) {
                    ui.meeting = new MeetingWindow(target);
                } else {
                    ui.meeting.present();
                }
            }
        });

        closeDown = new Action("quit", "Quit", "Exit the program", Stock.CLOSE);
        // AccelMap.changeEntry("<ZonesWindow>/Quit", Keyval.q,
        // ModifierType.CONTROL_MASK, true);
        //
        // closeDown.setAccelGroup(ag);
        // closeDown.setAccelPath("<ZonesWindow>/Quit");
        // closeDown.connectAccelerator();
        //
        closeDown.connect(new Action.ACTIVATE() {
            public void onActivate(Action source) {
                Gtk.mainQuit();
            }
        });

        popAbout = new Action("about", "About", "Details about this program", Stock.ABOUT);
        popAbout.connect(new Action.ACTIVATE() {
            public void onActivate(Action source) {
                final AboutDialog dialog;

                dialog = new AboutDialog();
                dialog.setProgramName("slashtime");
                dialog.setVersion(Version.VERSION);
                dialog.setComments("Show the time in various places!");
                dialog.setCopyright("Copyright 2003-2008 Operational Dynamics Consulting Pty Ltd, and Others\n"
                        + "A tiny java-gnome application originally written by Andrew Cowie");
                dialog.setAuthors(new String[] {
                    "Andrew Cowie <andrew@operationaldynamics.com>",
                });
                dialog.setLogo(images.marble);

                dialog.run();

                dialog.hide();
            }
        });

        /*
         * And code the popup menu:
         */

        view.connect(new Widget.BUTTON_PRESS_EVENT() {
            public boolean onButtonPressEvent(Widget source, EventButton event) {
                if (event.getButton() == MouseButton.RIGHT) {
                    menu.popup();
                }
                return false;
            }
        });

        menu = new Menu();

        Image m = new Image(images.calendar);

        ImageMenuItem pm = new ImageMenuItem(m, "");
        popMeeting.connectProxy(pm);

        menu.append(pm);
        menu.append(popAbout.createMenuItem());
        menu.append(closeDown.createMenuItem());

        menu.showAll();
        // has to be after map to screen
        selection.unselectAll();
    }

    private static final String DARK = "#777777";

    private static final String GRAY = "#A1A1A1";

    private static final String LIGHT = "#DDDDDD";

    private static final String BLUE = "blue";

    private static final String GREEN = "#2fb925";

    private static final String WHITE = "white";

    private static final String BLACK = "black";

    private void populate() {
        final Place[] places;

        places = loadPlaceList();

        for (int i = 0; i < places.length; i++) {
            final TreeIter pointer;
            final StringBuilder location;

            pointer = model.appendRow();

            /*
             * City and country
             */

            location = new StringBuilder();
            location.append(places[i].getCity());
            location.append("\n");
            location.append("<span size='x-small' color='");
            location.append(GRAY);
            location.append("'>");
            location.append(places[i].getCountry());
            location.append(" "); // necessary or row grows abnormally
            location.append("</span>");

            model.setValue(pointer, placeMarkup, location.toString());
            model.setValue(pointer, placeObject, places[i]);

            if (places[i].isLocal()) {
                current = places[i];
                model.setValue(pointer, iconImage, images.local);
            } else if (places[i].isHome()) {
                model.setValue(pointer, iconImage, images.home);
            } else if (places[i].isZulu()) {
                model.setValue(pointer, iconImage, images.gmt);
            }
        }

        if (current == null) {
            current = places[0]; // FIXME set to UTC directly
        }

        target = current;
    }

    void updateNow() {
        update(System.currentTimeMillis() / 1000);
    }

    void update(long when) {
        final int center;
        final TreeIter pointer;

        setTimeZone(current.getZoneName());
        center = calculateOffset(when);

        if (false) {
            System.out.println("update(" + formatTime("%e %b %y %H:%M:%S", when) + ")");
        }

        pointer = model.getIterFirst();
        do {
            final Place p;
            final StringBuffer time, offset;
            final int hours, minutes, fromGMT, halves;
            final String code;
            int halvesSinceMidnight;

            p = (Place) model.getValue(pointer, placeObject);

            setTimeZone(p.getZoneName());

            /*
             * Time, day, and date
             */

            time = new StringBuffer();
            time.append(formatTime("%H:%M", when));

            // before we go any further, extract hours and minutes
            hours = Integer.parseInt(time.substring(0, 2));
            minutes = Integer.parseInt(time.substring(3, 5));

            time.append("\n");

            time.append("<span size='x-small' font_desc='Mono' color='");
            time.append(GRAY);
            time.append("'>");
            /*
             * A note about this format string: in a monospace font, space
             * characters are really wide. Because we use %e for day of the
             * month (which is space padded if only one digit), the output
             * appears odd if we use "%a, %e". We can get away with loosing
             * the space, however, because commas in monospace are also wide;
             * testing shows that it provides sufficient whitespace between
             * the %a (three character day of the week) and the %e (two digit
             * day of the month) if that day is the 10th of the month or
             * greater.
             */
            time.append(formatTime("%a,%e %b %y", when));
            /*
             * The trailing space, on the other hand, acts as a spacer to
             * ensure that the date output and the timezone code aren't too
             * squished together.
             */
            time.append(" ");
            time.append("</span>"); // size

            model.setValue(pointer, timeMarkup, time.toString());

            /*
             * Offset and zone code
             */

            fromGMT = calculateOffset(when);
            halves = fromGMT - center;

            offset = new StringBuffer();

            // switch to Times New Roman for a clearer +/-
            offset.append("<span font_desc='Times New Roman'>");
            offset.append(Math.abs(halves) > 19 ? "" : " ");

            offset.append("<span size='x-small' rise='2000'>");
            if (halves == 0) {
                offset.append(" ");
            } else {
                offset.append(halves > 0 ? "+" : "-");
            }
            offset.append("</span>");
            offset.append("</span>");

            offset.append(Math.abs(halves / 2));

            offset.append("<span font_desc='Mono'>");
            offset.append(halves % 2 == 0 ? "" : "\u00bd");
            offset.append("</span>");
            offset.append("\n");

            offset.append("<span size='x-small' font_desc='Mono' color='");
            offset.append(GRAY);
            offset.append("'>");
            code = formatTime("%Z", when);
            if (code.length() == 3) {
                offset.append(" ");
            }

            offset.append(code);

            halvesSinceMidnight = hours * 2 + (minutes >= 30 ? 1 : 0);

            String foreground;
            String background;

            if (p.isWorkHours(halvesSinceMidnight)) {
                if (p.isLocal()) {
                    foreground = BLUE;
                } else if (p.isZulu()) {
                    foreground = GREEN;
                } else {
                    foreground = BLACK;
                }
                background = WHITE;
            } else if (p.isCivilHours(halvesSinceMidnight)) {
                if (p.isLocal()) {
                    foreground = BLUE;
                } else if (p.isZulu()) {
                    foreground = GREEN;
                } else {
                    foreground = BLACK;
                }
                background = LIGHT;
            } else {
                if (p.isLocal()) {
                    foreground = "#32fdff";
                } else if (p.isZulu()) {
                    foreground = "#a0ff97";
                } else {
                    foreground = BLACK;
                }
                background = DARK;
            }
            model.setValue(pointer, rowColor, foreground);
            model.setValue(pointer, rowBackground, background);

            offset.append("</span>");

            model.setValue(pointer, offsetMarkup, offset.toString());

            /*
             * Adjust the wall time to align with the hacker's day!
             */
            if (halvesSinceMidnight < 3) {
                halvesSinceMidnight += 48;
            }
            model.setValue(pointer, timeSort, halvesSinceMidnight);
            model.setValue(pointer, offsetSort, fromGMT);

        } while (pointer.iterNext());
    }

    /**
     * Tell the model to sort according to the displayed time [adjusted to
     * match the real hacker's day (which is until 01:30)]. This is the
     * default ordering.
     */
    void sortByWallTime() {
        sorted.setSortColumn(timeSort, SortType.ASCENDING);
    }

    /**
     * Tell the model to sort according to the offset. This is the original
     * behaviour of slashtime, and preserved for use during meeting setup so
     * that the Places aren't flipping all over the place inconsisently.
     */
    void sortByOffset() {
        vertical.setSortColumn(offsetSort);
    }

    Place getCurrent() {
        return current;
    }

    void toggle() {
        if (up) {
            window.hide();
            up = false;
        } else {
            window.present();
            up = true;
        }
    }

    /**
     * Work out the offset associated with this Place at a given time.
     * 
     * @param when
     *            the Date for which you're needing the offset. This matters
     *            because it has to figure out whether or not it's in DST.
     * @return the number of <b>half</b> hours by which this timezone is
     *         offset from UTC
     */
    static int calculateOffset(long when) {
        String rfc822;
        int raw, hours, halves;

        rfc822 = formatTime("%z", when);

        // stupidity: parseInt doesn't understand + but it does understand -
        if (rfc822.charAt(0) == '+') {
            rfc822 = rfc822.substring(1);
        }
        raw = Integer.parseInt(rfc822);

        hours = raw / 100;
        halves = hours * 2;
        if ((raw % 100) != 0) {
            if (raw > 0) {
                halves++;
            } else if (raw < 0) {
                halves--;
            }
        }
        return halves;
    }
}

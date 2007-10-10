/*
 * ZonesWindow.java
 * 
 * Copyright (c) 2006-2007 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package com.operationaldynamics.slashtime;

import static org.gnome.gtk.Alignment.CENTER;
import static org.gnome.gtk.Alignment.LEFT;
import static org.gnome.gtk.Alignment.TOP;

import java.io.FileNotFoundException;

import org.gnome.gdk.Color;
import org.gnome.gdk.Event;
import org.gnome.gdk.EventFocus;
import org.gnome.gdk.Pixbuf;
import org.gnome.gtk.Alignment;
import org.gnome.gtk.CellRendererPixbuf;
import org.gnome.gtk.CellRendererText;
import org.gnome.gtk.DataColumn;
import org.gnome.gtk.DataColumnInteger;
import org.gnome.gtk.DataColumnPixbuf;
import org.gnome.gtk.DataColumnReference;
import org.gnome.gtk.DataColumnString;
import org.gnome.gtk.Gtk;
import org.gnome.gtk.ListStore;
import org.gnome.gtk.Menu;
import org.gnome.gtk.StateType;
import org.gnome.gtk.TreeIter;
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
    private Window window;

    private VBox top;

    private TreeView view;

    private TreeViewColumn vertical;

    private ListStore model;

    private TreeSelection selection;

    private Menu menu;

    private Pixbuf icon;

    private Pixbuf marble;

    private Pixbuf home;

    private Pixbuf meeting;

    private DataColumnPixbuf iconImage;

    private DataColumnString placeMarkup;

    private DataColumnString timeMarkup;

    private DataColumnInteger timeSort;

    private DataColumnString offsetMarkup;

    private DataColumnInteger offsetSort;

    private DataColumnString rowColor;

    private DataColumnString rowBackground;

    private DataColumnReference placeObject;

    private NativeTime nt;

    private Place current;

    private Place target;

    /**
     * Build the main GUI window
     */
    ZonesWindow() {
        window = new Window();

        try {
            icon = new Pixbuf("share/pixmaps/marble.png");
            window.setIcon(icon);

            marble = new Pixbuf("share/pixmaps/marble.png", 23, 23, true);
            home = new Pixbuf("share/pixmaps/home.png", 24, 24, true);
            meeting = new Pixbuf("share/pixmaps/meeting.png", 20, 20, true);
        } catch (FileNotFoundException fnfe) {
            System.err.println("Icon file not found");
        }

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

        view = new TreeView(model);
        // view.setRulesHint(false);
        view.setHeadersVisible(false);
        view.setEnableSearch(false);

        /*
         * Unusually, we can pack all the CellRenderers into one
         * TreeViewColumn as they reserve a constant width per actual row.
         * This has the nice side effect of eliminating the one pixel boundary
         * between the former TreeViewColumns whose headings we weren't
         * looking at anyway.
         */

        CellRendererPixbuf image;
        CellRendererText text;

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

                if (Master.meeting == null) {
                    updateNow();
                } else {
                    Master.meeting.setPlace(current);
                }
            }
        });

        selection = view.getSelection();

        selection.connect(new TreeSelection.CHANGED() {
            public void onChanged(TreeSelection source) {
                final TreeIter row;

                row = selection.getSelected();

                if (row != null) {
                    target = (Place) model.getValue(row, placeObject);
                } else {
                    target = current;
                }

                if (Master.meeting != null) {
                    Master.meeting.setPlace(target);
                }
            }
        });

        nt = new NativeTime();

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

        int s_w = window.getScreen().getWidth();
        int s_h = window.getScreen().getHeight();

        int w = window.getWidth();
        int h = window.getHeight();

        window.move(s_w - w - 20, s_h - h - 30);
        window.present();

        /*
         * Fire up the interrupt timer to update the time readouts.
         */

        Thread clock = new Thread() {
            private long last = -1;

            public void run() {
                while (true) {
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException ie) {
                        continue;
                    }

                    if (Master.meeting != null) {
                        continue;
                    }

                    long tick = System.currentTimeMillis() / 1000;

                    if ((tick % 60) == 0) {
                        update(tick);
                    } else if ((last + 1) != tick) {
                        /*
                         * There is the annoying case that if we've come back
                         * from a suspend the time display is wrong until the
                         * next minute occurs. So we keep track of the
                         * previous tick and if it isn't one less than the
                         * current one we call the update method.
                         */
                        update(tick);
                    }
                    last = tick;
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
        // final Action popMeeting = new Action("meeting", "Meeting", "Pop up
        // the Meeting planner",
        // GtkStockItem.GO_FORWARD.getString());
        // AccelMap.changeEntry("<ZonesWindow>/Meeting", KeyValue.M,
        // ModifierType.CONTROL_MASK, true);
        //
        // popMeeting.setAccelGroup(ag);
        // popMeeting.setAccelPath("<ZonesWindow>/Meeting");
        // popMeeting.connectAccelerator();
        //
        // popMeeting.addListener(new ActionListener() {
        // public void actionEvent(ActionEvent action) {
        // if (action.getType() == ActionEvent.Type.ACTIVATE) {
        // /*
        // * And the logic to pop up a MeetingWindow. This seems
        // * quite burried, and a bit odd to use Action's activate
        // * to fire this. Perhaps we'll get something out of
        // * leaving it here.
        // */
        // if (Master.meeting == null) {
        // Master.meeting = new MeetingWindow(target);
        // } else {
        // Master.meeting.present();
        // }
        // }
        // }
        // });
        //
        // Action closeDown = new Action("quit", "Quit", "Exit the program",
        // GtkStockItem.CLOSE.getString());
        // AccelMap.changeEntry("<ZonesWindow>/Quit", KeyValue.Q,
        // ModifierType.CONTROL_MASK, true);
        //
        // closeDown.setAccelGroup(ag);
        // closeDown.setAccelPath("<ZonesWindow>/Quit");
        // closeDown.connectAccelerator();
        //
        // closeDown.addListener(new ActionListener() {
        // public void actionEvent(ActionEvent action) {
        // if (action.getType() == ActionEvent.Type.ACTIVATE) {
        // Gtk.mainQuit();
        // }
        // }
        // });
        //
        // /*
        // * And code the popup menu:
        // */
        //
        // view.addListener(new MouseListener() {
        // public boolean mouseEvent(MouseEvent event) {
        // if (event.getType() == MouseEvent.Type.BUTTON_PRESS) {
        // if (event.getButtonPressed() == MouseEvent.BUTTON3) {
        // menu.popup();
        // }
        // }
        // return false;
        // }
        // });
        //
        // menu = new Menu();
        //
        // Image meeting_image = new Image(meeting);
        // ImageMenuItem meeting = new ImageMenuItem("Meeting...", false);
        // meeting.setImage(meeting_image);
        // meeting.addListener(new MenuItemListener() {
        // public void menuItemEvent(MenuItemEvent event) {
        // // use the action we created for the accelerator, rather than
        // // duplicate that logic here.
        // popMeeting.activate();
        // }
        // });
        // menu.append(meeting);
        //
        // Image about_image = new Image(GtkStockItem.ABOUT, IconSize.MENU);
        // ImageMenuItem about = new ImageMenuItem("About", false);
        // about.setImage(about_image);
        // about.addListener(new MenuItemListener() {
        // public void menuItemEvent(MenuItemEvent event) {
        // AboutDialog about_dialog = new AboutDialog();
        // about_dialog.setName("slashtime");
        // about_dialog.setVersion(Version.VERSION);
        // about_dialog.setComments("Show the time in various places!");
        // about_dialog.setCopyright("Copyright 2003-2007 Operational Dynamics
        // Consulting Pty Ltd\n"
        // + "Written in java-gnome by Andrew Cowie
        // <andrew@operationaldynamics.com>\n");
        // // about_dialog.setAuthors(new String[] {
        // // "Andrew Cowie <andrew@operationaldynamics.com>",
        // // });
        // about_dialog.setLogo(icon);
        // about_dialog.setIcon(icon);
        //
        // about_dialog.run();
        //
        // about_dialog.destroy();
        // }
        //
        // });
        // menu.append(about);
        //
        // Image quit_image = new Image(GtkStockItem.QUIT, IconSize.MENU);
        // ImageMenuItem quit = new ImageMenuItem("Quit", false);
        // quit.setImage(quit_image);
        // quit.addListener(new MenuItemListener() {
        // public void menuItemEvent(MenuItemEvent event) {
        // Gtk.mainQuit();
        // }
        // });
        // menu.append(quit);
        //
        // menu.showAll();
        // has to be after map to screen
        selection.unselectAll();
        vertical.clicked();
    }

    private static final String DARK = "#777777";

    private static final String GRAY = "#A1A1A1";

    private static final String LIGHT = "#DDDDDD";

    private static final String BLUE = "blue";

    private static final String GREEN = "#2fb925";

    private static final String WHITE = "white";

    private static final String BLACK = "black";

    private void populate() {
        // mock data!

        Place[] mock = new Place[] {
                new Place("UTC", "Zulu", "Universal Time"),
                new Place("America/Montreal", "Toronto", "Canada"),
                new Place("America/Vancouver", "Vancouver", "Canada"),
                new Place("Australia/Sydney", "Sydney", "Australia"),
                new Place("Europe/Paris", "Paris", "France"),
                new Place("America/Halifax", "Fredericton", "Canada"),
                new Place("Europe/London", "London", "UK"),
                new Place("Asia/Calcutta", "Bangalore", "India"),
                new Place("Asia/Hong_Kong", "Hong Kong", "China"),
                new Place("Pacific/Auckland", "Auckland", "New Zealand"),
                new Place("Pacific/Honolulu", "Hawaii", "USA"),
                new Place("America/Los_Angeles", "Los Angeles", "USA"),
                new Place("America/New_York", "New York", "USA"),
                new Place("America/Edmonton", "Calgary", "Canada"),
                new Place("Australia/Adelaide", "Adelaide", "Australia"),
                new Place("Asia/Tokyo", "Tokyo", "Japan"),
                new Place("Asia/Singapore", "Singapore", "Singapore"),
                new Place("Asia/Dubai", "Dubai", "UAE"),
                new Place("Australia/Perth", "Perth", "Australia"),
        };

        for (int i = 0; i < mock.length; i++) {
            TreeIter pointer = model.appendRow();

            /*
             * City and country
             */

            StringBuffer place = new StringBuffer();
            place.append(mock[i].getCity());
            place.append("\n");
            place.append("<span size='x-small' color='");
            place.append(GRAY);
            place.append("'>");
            place.append(mock[i].getCountry());
            place.append(" "); // necessary or row grows abnormally
            place.append("</span>");

            model.setValue(pointer, placeMarkup, place.toString());
            model.setValue(pointer, placeObject, mock[i]);

            if (mock[i].isLocal()) {
                current = mock[i];
                model.setValue(pointer, iconImage, home);
            } else if (mock[i].isZulu()) {
                model.setValue(pointer, iconImage, marble);
            }
        }

        if (current == null) {
            current = mock[0]; // FIXME set to UTC directly
        }

        target = current;
    }

    void updateNow() {
        update(System.currentTimeMillis() / 1000);
    }

    void update(long when) {
        nt.setTimeZone(current.getZoneName());
        int center = nt.calculateOffset(when);

        TreeIter pointer = model.getIterFirst();
        do {
            Place p = (Place) model.getValue(pointer, placeObject);

            nt.setTimeZone(p.getZoneName());

            /*
             * Time, day, and date
             */

            StringBuffer time = new StringBuffer();
            time.append(nt.format("%H:%M", when));

            // before we go any further, extract hours and minutes
            int hours = Integer.parseInt(time.substring(0, 2));
            int minutes = Integer.parseInt(time.substring(3, 5));

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
            time.append(nt.format("%a,%e %b %y", when));
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

            int fromGMT = nt.calculateOffset(when);
            int halves = fromGMT - center;

            StringBuffer offset = new StringBuffer();

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
            String code = nt.format("%Z", when);
            if (code.length() == 3) {
                offset.append(" ");
            }

            offset.append(code);

            int halvesSinceMidnight = hours * 2 + (minutes >= 30 ? 1 : 0);

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
        vertical.setSortColumn(timeSort);
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
}

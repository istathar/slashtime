/*
 * MeetingWindow.java
 * 
 * Copyright (c) 2006-2007 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package com.operationaldynamics.slashtime;

import java.io.FileNotFoundException;

import org.gnome.gdk.Event;
import org.gnome.gdk.Pixbuf;
import org.gnome.gtk.Calendar;
import org.gnome.gtk.HBox;
import org.gnome.gtk.HScale;
import org.gnome.gtk.Label;
import org.gnome.gtk.VBox;
import org.gnome.gtk.Widget;
import org.gnome.gtk.Window;
import org.gnome.gtk.WindowPosition;

class MeetingWindow
{
    private Window     window;
    private VBox       top;
    private Label      placeTime;
    private Label      placeDate;
    private Label      placeCity;
    private Label      placeCountry;
    private Calendar   calendar;
    private HScale     hour;
    private HScale     minute;

    private Place      current;
    private long       when = -1;

    private NativeTime nt;

    private Label createTextLabel(String text, boolean italics) {
        StringBuffer buf = new StringBuffer();
        if (italics) {
            buf.append("<span font_desc='Times New Roman' size='large' style='italic'>");
        }
        buf.append(text);
        if (italics) {
            buf.append("</span>");
        }
        Label l = new Label(buf.toString());
        l.setUseMarkup(true);
        l.setAlignment(0.0, 0.5);
        return l;
    }

    private Label createDisplayLabel() {
        Label l = new Label("");
        l.setAlignment(0.5, 0.5);
        l.setWidthChars(20);
        l.setUseMarkup(true);
        return l;
    }

    MeetingWindow(Place where) {
        Master.zones.sortByOffset();

        window = new Window();
        window.setTitle("Find a meeting time");
        window.setDecorated(true);
        window.setPosition(WindowPosition.CENTER);

        try {
            Pixbuf icon = new Pixbuf("share/pixmaps/meeting.png");
            window.setIcon(icon);
        } catch (FileNotFoundException fnfe) {
            System.err.println("Icon file not found");
        } catch (Exception e) {
            // because JGException is the stupidest thing I've ever heard of
        }

        top = new VBox(false, 0);
        Label l;

        /*
         * Display the location
         */
        l = createTextLabel("Set the time at:", false);
        top.packStart(l, false, false, 0);

        placeCity = createDisplayLabel();
        placeCountry = createDisplayLabel();

        VBox centering = new VBox(false, 0);
        centering.packStart(placeCity, true, false, 0);
        centering.packStart(placeCountry, true, false, 0);
        top.packStart(centering, false, false, 3);

        /*
         * Describe the time
         */

        l = createTextLabel("to:", false);
        top.packStart(l, false, false, 0);

        placeTime = createDisplayLabel();
        top.packStart(placeTime, true, false, 0);

        placeDate = createDisplayLabel();
        top.packStart(placeDate, true, false, 0);

        /*
         * further instruction
         */

        l = createTextLabel("Click on a city to change location", true);
        top.packStart(l, false, false, 3);

        /*
         * Initialize the calendar, and pack into an HBox so that it doesn't get
         * stretched if the window becomes wider.
         */
        calendar = new Calendar();

        calendar.addListener(new CalendarListener() {
            public void calendarEvent(CalendarEvent event) {
                if (event.getType() == CalendarEvent.Type.DAY_SELECTED) {
                    update();
                }
            }
        });

        HBox spacer = new HBox(false, 0);
        spacer.packStart(calendar, true, false, 0);

        top.packStart(spacer, false, false, 3);

        /*
         * Sliders
         */

        hour = new HScale(0, 23, 1);
        minute = new HScale(0, 59, 15);
        minute.setDigits(0);

        hour.addListener(new RangeListener() {
            public void rangeEvent(RangeEvent event) {
                if (event.getType() == RangeEvent.Type.VALUE_CHANGED) {
                    if (hour.hasFocus()) {
                        update();
                    }
                }
            }
        });
        minute.addListener(new RangeListener() {
            public void rangeEvent(RangeEvent event) {
                if (event.getType() == RangeEvent.Type.VALUE_CHANGED) {
                    if (minute.hasFocus()) {
                        update();
                    }
                }
            }
        });

        top.packStart(hour, false, false, 0);
        top.packStart(minute, false, false, 0);

        /*
         * Initialize the date, hour and minute
         */
        nt = new NativeTime();
        setPlace(where);
        update();

        /*
         * Finish up and present
         */
        window.add(top);
        window.showAll();
        window.present();

        window.connect(new Window.DELETE_EVENT() {
            public boolean onDeleteEvent(Widget source, Event event) {
                when = -1;
                Master.meeting = null;
                Master.zones.sortByWallTime();
                Master.zones.updateNow();
                return false;
            }
        });
    }

    /**
     * Update this Place shown in this MeetingWindow.
     */
    void setPlace(Place p) {
        String city, country;

        if (p == null) {
            throw new NullArgumentException();
        } else if (p.isZulu()) {
            city = "Greenwich";
            country = "Universal Time, Co-ordinated";
        } else {
            city = p.getCity();
            country = p.getCountry();
        }

        StringBuffer buf = new StringBuffer();
        buf.append("<span size='xx-large'>");
        buf.append(city);
        buf.append("</span>");
        placeCity.setLabel(buf.toString());

        placeCountry.setMarkup(country);

        this.current = p;

        if (when == -1) {
            when = System.currentTimeMillis() / 1000;
        }

        nt.setTimeZone(p.getZoneName());

        int i = Integer.parseInt(nt.format("%H", when));
        hour.setValue(i);
        int j = Integer.parseInt(nt.format("%M", when));
        minute.setValue(j);

        /*
         * and now that a place is set, reset the ZonesWindow.
         */
        update();
    }

    /**
     * Update to the time data displayed ZonesWindow by propegating the settings
     * from this Window.
     */
    private void update() {
        double h = hour.getValue();
        int i = (int) Math.round(h);

        double m = minute.getValue();
        int j = (int) Math.round(m);

        int[] ymd = calendar.getDate();

        /*
         * God knows what gtk_calendar_get_date() does to the environment, so
         * reset the timezone here:
         */
        nt.setTimeZone(current.getZoneName());

        when = nt.makeTick(ymd[0], ymd[1], ymd[2], i, j);

        placeTime.setLabel("<big><b>" + nt.format("%H:%M", when) + "</b></big>");
        placeDate.setLabel(nt.format("%a, %e %b %y", when));

        Master.zones.update(when);
    }

    void present() {
        window.present();
    }
}

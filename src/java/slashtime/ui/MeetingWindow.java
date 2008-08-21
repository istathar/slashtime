/*
 * MeetingWindow.java
 * 
 * Copyright (c) 2006-2008 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package slashtime.ui;

import org.gnome.gdk.Event;
import org.gnome.gtk.Calendar;
import org.gnome.gtk.HBox;
import org.gnome.gtk.HScale;
import org.gnome.gtk.Label;
import org.gnome.gtk.Range;
import org.gnome.gtk.VBox;
import org.gnome.gtk.Widget;
import org.gnome.gtk.Window;
import org.gnome.gtk.WindowPosition;

import slashtime.domain.Place;
import slashtime.util.NullArgumentException;

import static org.freedesktop.bindings.Time.formatTime;
import static org.freedesktop.bindings.Time.makeTime;
import static org.freedesktop.bindings.Time.setTimeZone;
import static org.gnome.gtk.Alignment.CENTER;
import static org.gnome.gtk.Alignment.LEFT;
import static slashtime.client.Master.ui;

class MeetingWindow
{
    private Window window;

    private VBox top;

    private Label placeTime;

    private Label placeDate;

    private Label placeCity;

    private Label placeCountry;

    private Calendar calendar;

    private HScale hour;

    private HScale minute;

    private Place current;

    private long when = -1;

    private static Label createTextLabel(String text, boolean italics) {
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
        l.setAlignment(LEFT, CENTER);
        return l;
    }

    private static Label createDisplayLabel() {
        Label l = new Label("");
        l.setAlignment(CENTER, CENTER);
        l.setWidthChars(20);
        l.setUseMarkup(true);
        return l;
    }

    MeetingWindow(Place where) {
        ui.zones.indicateWrongTime();

        window = new Window();
        window.setTitle("Find a meeting time");
        window.setDecorated(true);
        window.setPosition(WindowPosition.CENTER);
        window.setTransientFor(ui.zones.getWindow());

        window.setIcon(images.calendar);

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
         * Initialize the calendar, and pack into an HBox so that it doesn't
         * get stretched if the window becomes wider.
         */
        calendar = new Calendar();

        calendar.connect(new Calendar.DaySelected() {
            public void onDaySelected(Calendar source) {
                update();
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
        hour.connect(new Range.ValueChanged() {
            public void onValueChanged(Range source) {
                if (hour.getHasFocus()) {
                    update();
                }
            }
        });
        minute.connect(new Range.ValueChanged() {
            public void onValueChanged(Range source) {
                if (minute.getHasFocus()) {
                    update();
                }
            }
        });
        top.packStart(hour, false, false, 0);
        top.packStart(minute, false, false, 0);

        /*
         * Initialize the date, hour and minute
         */
        setPlace(where);
        update();

        /*
         * Finish up and present
         */
        window.add(top);
        window.showAll();
        window.present();

        window.connect(new Window.DeleteEvent() {
            public boolean onDeleteEvent(Widget source, Event event) {
                when = -1;
                ui.meeting = null;
                ui.zones.indicateCorrectTime();
                ui.zones.updateNow();
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

        placeCountry.setLabel(country);

        this.current = p;

        if (when == -1) {
            when = System.currentTimeMillis() / 1000;
            when -= when % 3600;
        }

        setTimeZone(p.getZoneName());

        int i = Integer.parseInt(formatTime("%H", when));
        hour.setValue(i);
        int j = Integer.parseInt(formatTime("%M", when));
        minute.setValue(j);

        /*
         * and now that a place is set, reset the ZonesWindow.
         */
        update();
    }

    /**
     * Update to the time data displayed ZonesWindow by propegating the
     * settings from this Window.
     */
    private void update() {
        double h = hour.getValue();
        int i = (int) Math.round(h);

        double m = minute.getValue();
        int j = (int) Math.round(m);

        int year = calendar.getDateYear();
        int month = calendar.getDateMonth();
        int day = calendar.getDateDay();

        /*
         * God knows what gtk_calendar_get_date() does to the environment, so
         * reset the timezone here:
         */
        setTimeZone(current.getZoneName());

        when = makeTime(year, month, day, i, j, 0);

        placeTime.setLabel("<big><b>" + formatTime("%H:%M", when) + "</b></big>");
        placeDate.setLabel(formatTime("%a, %e %b %y", when));

        ui.zones.update(when);
    }

    void present() {
        window.present();
    }
}

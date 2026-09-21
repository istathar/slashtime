use clap::{value_parser, Arg, ArgAction, Command};
use eframe::egui;
use slashtime::{
    days_in_month, find_local, format_date, format_day, format_line, format_offset_parts,
    format_time, Band, Locality,
};
use std::path::PathBuf;
use tz::{TzError, UtcDateTime};

// The shading says how reachable someone is at their hour, so the list
// deliberately ignores the desktop theme; here colour is data, not chrome.
const WORK: egui::Color32 = egui::Color32::from_rgb(0xff, 0xff, 0xff);
const CIVIL: egui::Color32 = egui::Color32::from_rgb(0xdd, 0xdd, 0xdd);
const NIGHT: egui::Color32 = egui::Color32::from_rgb(0x77, 0x77, 0x77);

const SUBDUED: egui::Color32 = egui::Color32::from_rgb(0xa1, 0xa1, 0xa1);
const PLAIN: egui::Color32 = egui::Color32::from_rgb(0x00, 0x00, 0x00);
const LOCAL: egui::Color32 = egui::Color32::from_rgb(0x00, 0x00, 0xff);
const ZULU: egui::Color32 = egui::Color32::from_rgb(0x2f, 0xb9, 0x25);
const LOCAL_DARK: egui::Color32 = egui::Color32::from_rgb(0x32, 0xfd, 0xff);
const ZULU_DARK: egui::Color32 = egui::Color32::from_rgb(0xa0, 0xff, 0x97);
const HOVER: egui::Color32 = egui::Color32::from_rgb(0x1c, 0x71, 0xd8);

// How much of that colour is washed over the row under the pointer. The band
// underneath is the data, so it is tinted rather than replaced: white, grey
// and dark stay in that order, and every foreground goes on reading as it did.
const HOVER_OPACITY: f32 = 0.25;

// the frame turns red while a meeting is being planned, so the list is never
// mistaken for the actual time somewhere
const WRONG: egui::Color32 = egui::Color32::from_rgb(0xd0, 0x18, 0x18);

// The Application Identifier has to match the basename of the installed
// .desktop file so that the wayland compositor works out which icon belongs
// to the window.
//
// eframe only sends an Application Identifier when built with the "wayland"
// feature enabled, which Cargo.toml does explicitly.
const APP_ID: &str = "org.aesiniath.Slashtime";

// an aspect ratio of approximately 7:1 given a list of 20 or so cities.
const WIDTH: f32 = 272.0;

const VALUE_SIZE: f32 = 14.7;
const CAPTION_SIZE: f32 = 9.5;
const EDGE: f32 = 4.0;

// Clear space wanted between the text and the edge of its coloured band, and
// between the two lines of a row. PADDING has to be the larger of the two, or
// a row stops reading as one thing and the list dissolves into stripes. Both
// are distances to the ink rather than to the line box.
const PADDING: f32 = 6.0;
const SEPARATION: f32 = 4.0;

// How tall a line box is, and where the ink sits inside it, for the face and
// the two sizes above. These belong to the face rather than to the point size,
// which is why the program insists on one face instead of taking whatever the
// machine happens to have: it makes the whole vertical layout a constant.
const VALUE_LINE: f32 = 20.03125;
const VALUE_ABOVE: f32 = 4.0;
const VALUE_BELOW: f32 = 4.03125;
const CAPTION_LINE: f32 = 12.9375;
const CAPTION_ABOVE: f32 = 3.0;
const CAPTION_BELOW: f32 = 0.4375;

// How wide the fixed pieces of the upper line are at the value size, for the
// same reason. The weekday slot is the widest of the seven, which in this face
// is Mon; Wed is a hair narrower. The ½ gets a slot of its own whether or not
// a zone has one, so that the units column stays put all the way down.
const WEEKDAY_WIDTH: f32 = 39.0625;
const CLOCK_WIDTH: f32 = 37.5625;
const HALF_WIDTH: f32 = 11.34375;

// Take the slack the face already carries out of each gap, so that what is
// left is the clear space actually asked for above.
const LEAD: f32 = PADDING - VALUE_ABOVE;
const TO_CAPTION: f32 = VALUE_LINE - VALUE_BELOW + SEPARATION - CAPTION_ABOVE;
const TRAIL: f32 = PADDING - CAPTION_BELOW;
const ROW_HEIGHT: f32 = LEAD + TO_CAPTION + CAPTION_LINE + TRAIL;

// the bar down the right hand edge that says a row is selected. It sits in
// the margin the offset column already keeps clear, and runs the full height
// so that a run of selected rows reads as one stroke.
const MARK: f32 = 3.0;

// the icon sits in a reserved column at the left, so that the city names line
// up whether or not a given row has one.
const ICON_COLUMN: f32 = 34.0;
const ICON_SIZE: f32 = 20.0;

// width set aside at the right hand end for the offset and the zone code,
// which the time and date are then right aligned against.
const OFFSET_COLUMN: f32 = 50.0;

// We use Noto Sans as a widely accepted libre font. The key metrics
// consideration is that its numerals have a uniform width (without having the
// blockiness of a typewriter face) so the clock does not shuffle sideways as
// the minutes turn over.
//
// The font is embedded rather than looked for on the machine. Every face has
// its own line heights, so taking whatever happened to be installed would
// mean measuring at startup and laying out differently from one machine to
// the next; with the face fixed, the vertical layout above is a constant.
// Pinned to Regular and subset to Latin, it costs 29kB rather than the 712kB
// of the full variable font; names in other scripts are drawn from fonts the
// system has. See share/fonts/OFL.txt.
const FACE: &[u8] = include_bytes!("../../share/fonts/NotoSans-Regular-subset.ttf");

// Put the face at the head of the family, then any system fonts the city and
// country names need, leaving egui's built in fonts behind them to cover
// anything still missing. Row heights come from the first font in a family,
// so the fallbacks do not disturb the vertical layout.
fn install_fonts(ctx: &egui::Context, locations: &[Locality]) {
    let mut fonts = egui::FontDefinitions::default();

    let mut names = vec!["sans".to_string()];
    fonts.font_data.insert(
        "sans".to_string(),
        std::sync::Arc::new(egui::FontData::from_static(FACE)),
    );

    for (name, data) in fallback_fonts(locations) {
        fonts
            .font_data
            .insert(name.clone(), std::sync::Arc::new(data));
        names.push(name);
    }

    fonts
        .families
        .entry(egui::FontFamily::Proportional)
        .or_default()
        .splice(0..0, names);

    ctx.set_fonts(fonts);
}

// Ask the system which font it would use for each script that appears in the
// names but is missing from the embedded face. Usually nothing is missing and
// the system is never asked. Latin is skipped, as the platform's choice for it
// is some unrelated face, and so are Common and Inherited, which punctuation
// and combining marks belong to.
fn fallback_fonts(locations: &[Locality]) -> Vec<(String, egui::FontData)> {
    use skrifa::MetadataProvider;
    use unicode_script::{Script, UnicodeScript};

    let charmap = skrifa::FontRef::new(FACE).expect("embedded face").charmap();

    let mut scripts = Vec::new();
    for c in locations
        .iter()
        .flat_map(|place| place.city_name.chars().chain(place.country_name.chars()))
    {
        let script = c.script();
        if charmap.map(c).is_none()
            && !matches!(script, Script::Common | Script::Inherited | Script::Latin)
            && !scripts.contains(&script)
        {
            scripts.push(script);
        }
    }

    let mut result: Vec<(String, egui::FontData)> = Vec::new();
    if scripts.is_empty() {
        return result;
    }

    let mut collection = fontique::Collection::new(fontique::CollectionOptions::default());

    for script in scripts {
        let key = fontique::FallbackKey::new(
            fontique::Script::from_str_unchecked(script.short_name()),
            None,
        );
        let Some(id) = collection.fallback_families(key).next() else {
            continue;
        };
        let Some(family) = collection.family(id) else {
            continue;
        };

        // Han and Hangul, for one, usually resolve to the same family
        let name = family.name().to_string();
        if result.iter().any(|(n, _)| *n == name) {
            continue;
        }

        let Some(font) = family.default_font() else {
            continue;
        };
        let Some(blob) = font.load(None) else {
            continue;
        };

        let mut data = egui::FontData::from_owned(blob.as_ref().to_vec());
        data.index = font.index();
        result.push((name, data));
    }

    result
}

// the icons are embedded rather than read from disk; they are tiny, and this
// keeps the program from having on-disk dependencies.
const HOME_PNG: &[u8] = include_bytes!("../../share/slashtime/images/home.png");
const LOCAL_PNG: &[u8] = include_bytes!("../../share/slashtime/images/local.png");
const ZULU_PNG: &[u8] = include_bytes!("../../share/icons/hicolor/48x48/apps/slashtime.png");

struct Icons {
    home: egui::TextureHandle,
    local: egui::TextureHandle,
    zulu: egui::TextureHandle,
}

impl Icons {
    fn load(ctx: &egui::Context) -> Self {
        Icons {
            home: texture(ctx, "home", HOME_PNG),
            local: texture(ctx, "local", LOCAL_PNG),
            zulu: texture(ctx, "zulu", ZULU_PNG),
        }
    }

    // Which icon a row gets, in order of precedence. The pivot, which the
    // offsets are measured from, gets the local icon. A location in the
    // machine's own time zone, or the home location if one was given, gets
    // the home icon, so it can still be found after pivoting elsewhere. The
    // Zulu row comes last, as it is already picked out in green. Selection is
    // not an icon: it is a passing state of the list rather than something
    // true about a place, so it is drawn as a bar down the edge of the row
    // instead.
    fn choose(&self, reading: &Reading) -> Option<&egui::TextureHandle> {
        let location = reading.location;

        if reading.is_pivot {
            Some(&self.local)
        } else if location.is_local || location.is_home {
            Some(&self.home)
        } else if location.is_zulu {
            Some(&self.zulu)
        } else {
            None
        }
    }
}

fn texture(ctx: &egui::Context, name: &str, bytes: &[u8]) -> egui::TextureHandle {
    let decoded = image::load_from_memory(bytes)
        .expect("icon should be a readable png")
        .to_rgba8();

    let size = [decoded.width() as usize, decoded.height() as usize];
    let image = egui::ColorImage::from_rgba_unmultiplied(size, decoded.as_raw());

    ctx.load_texture(name, image, egui::TextureOptions::LINEAR)
}

// counting months as a single running number keeps the year in step
fn step_month(year: i32, month: u8, by: i32) -> (i32, u8) {
    let months = year * 12 + i32::from(month) - 1 + by;

    (months.div_euclid(12), (months.rem_euclid(12) + 1) as u8)
}

// A meeting is a wall clock reading on the pivot's clock, which the keys step
// back and forth. The instant it names is what the whole list is shown at.
struct Meeting {
    year: i32,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
}

impl Meeting {
    // the wall clock reading at a place at the given moment
    fn at(place: &Locality, when: &UtcDateTime) -> Option<Self> {
        let there = when.project(place.zone.as_ref()).ok()?;

        Some(Meeting {
            year: there.year(),
            month: there.month(),
            day: there.month_day(),
            hour: there.hour(),
            minute: there.minute(),
        })
    }

    // a new meeting starts at the top of the current hour on the place's
    // clock, as meetings are mostly held on the hour
    fn new(place: &Locality, now: &UtcDateTime) -> Option<Self> {
        let mut meeting = Meeting::at(place, now)?;

        meeting.minute = 0;

        Some(meeting)
    }

    fn instant(&self, place: &Locality) -> Option<UtcDateTime> {
        place
            .instant(self.year, self.month, self.day, self.hour, self.minute)
            .ok()
            .flatten()
    }

    // moving between months has to pull the day back when the new month is
    // shorter, or the date would not exist
    fn shift_month(&mut self, by: i32) {
        let (year, month) = step_month(self.year, self.month, by);

        self.year = year;
        self.month = month;
        self.day = self.day.min(days_in_month(year, month));
    }

    // Days are counted off against the length of each month in turn rather
    // than by adding to an instant, so that a day later means the same wall
    // clock reading the next day even across a daylight savings change.
    fn shift_day(&mut self, by: i32) {
        let (mut year, mut month) = (self.year, self.month);
        let mut day = i32::from(self.day) + by;

        while day < 1 {
            (year, month) = step_month(year, month, -1);
            day += i32::from(days_in_month(year, month));
        }

        while day > i32::from(days_in_month(year, month)) {
            day -= i32::from(days_in_month(year, month));
            (year, month) = step_month(year, month, 1);
        }

        self.year = year;
        self.month = month;
        self.day = day as u8;
    }

    fn shift_minute(&mut self, by: i32) {
        let moment = i32::from(self.hour) * 60 + i32::from(self.minute) + by;
        let (days, rest) = (moment.div_euclid(24 * 60), moment.rem_euclid(24 * 60));

        self.hour = (rest / 60) as u8;
        self.minute = (rest % 60) as u8;

        if days != 0 {
            self.shift_day(days);
        }
    }
}

// one location as it appears at a given moment, relative to a given pivot.
// The names are borrowed from the location; what is formatted here depends
// on the moment and the pivot, so it is recomputed each pass rather than
// cached and invalidated.
struct Reading<'a> {
    index: usize,
    location: &'a Locality,
    time: String,
    day: &'static str,
    date: String,
    offset: String,
    half: bool,
    abbreviation: &'a str,
    band: Band,
    key: u8,
    is_selected: bool,
    is_pivot: bool,
}

fn read<'a>(
    locations: &'a [Locality],
    pivot: usize,
    when: &UtcDateTime,
    selected: &[usize],
) -> Result<Vec<Reading<'a>>, TzError> {
    let mut readings = Vec::with_capacity(locations.len());
    let here = locations[pivot].offset(when)?;

    for (index, location) in locations.iter().enumerate() {
        let there = when.project(location.zone.as_ref())?;
        let offset = format_offset_parts(location.offset(when)? - here);

        readings.push(Reading {
            index,
            location,
            time: format_time(&there),
            day: format_day(there.week_day()),
            date: format_date(&there),
            offset: offset.0,
            half: offset.1,
            abbreviation: location.abbreviation(when)?,
            band: location.band(when)?,
            key: location.sort_key(when)?,
            is_selected: selected.contains(&index),
            is_pivot: index == pivot,
        });
    }

    readings.sort_by_key(|reading| reading.key);

    Ok(readings)
}

// The background shows whether it is working hours, civil hours, or night at
// that location, as Band classifies the time of day there. The text is blue
// for a location in the machine's own time zone and green for Zulu, each in a
// lighter shade over the night band so that it can still be read; every other
// row is black.
fn colours(reading: &Reading) -> (egui::Color32, egui::Color32) {
    let background = match reading.band {
        Band::Work => WORK,
        Band::Civil => CIVIL,
        Band::Night => NIGHT,
    };

    let dark = reading.band == Band::Night;

    let foreground = if reading.location.is_local {
        if dark {
            LOCAL_DARK
        } else {
            LOCAL
        }
    } else if reading.location.is_zulu {
        if dark {
            ZULU_DARK
        } else {
            ZULU
        }
    } else {
        PLAIN
    };

    (background, foreground)
}

// Each row is painted into an exact rectangle of the size we've statically
// determined (rather than laid out from its contents as full capability GUI
// toolkit like GTK would do), so that the shaded bands line up and reach both
// edges regardless of how long a city name happens to be.
fn row(ui: &mut egui::Ui, reading: &Reading, icons: &Icons) -> egui::Response {
    let (rect, response) = ui.allocate_exact_size(
        egui::vec2(ui.available_width(), ROW_HEIGHT),
        egui::Sense::click(),
    );

    let (background, foreground) = colours(reading);

    let value = egui::FontId::proportional(VALUE_SIZE);
    let caption = egui::FontId::proportional(CAPTION_SIZE);

    let upper = rect.top() + LEAD;
    let lower = upper + TO_CAPTION;

    let left = rect.left() + ICON_COLUMN;
    let middle = rect.right() - OFFSET_COLUMN;
    let right = rect.right() - EDGE;

    let painter = ui.painter();

    painter.rect_filled(rect, 0.0, background);

    // the row under the pointer gets a translucent layer of the hover colour,
    // so that the band beneath still shows through
    if response.hovered() {
        painter.rect_filled(rect, 0.0, HOVER.gamma_multiply(HOVER_OPACITY));
    }

    // Being selected outlasts the pointer, so it is put at the edge rather
    // than across the row: a state you chose should not go on shouting.
    if reading.is_selected {
        painter.rect_filled(
            egui::Rect::from_min_max(egui::pos2(rect.right() - MARK, rect.top()), rect.max),
            0.0,
            HOVER,
        );
    }

    if let Some(icon) = icons.choose(reading) {
        let centre = egui::pos2(rect.left() + ICON_COLUMN / 2.0, rect.center().y);

        painter.image(
            icon.id(),
            egui::Rect::from_center_size(centre, egui::vec2(ICON_SIZE, ICON_SIZE)),
            egui::Rect::from_min_max(egui::pos2(0.0, 0.0), egui::pos2(1.0, 1.0)),
            egui::Color32::WHITE,
        );
    }

    painter.text(
        egui::pos2(left, upper),
        egui::Align2::LEFT_TOP,
        &reading.location.city_name,
        value.clone(),
        foreground,
    );
    painter.text(
        egui::pos2(left, lower),
        egui::Align2::LEFT_TOP,
        &reading.location.country_name,
        caption.clone(),
        SUBDUED,
    );

    // The day of the week hangs off the end of the time, in a slot wide enough
    // for the widest of them so that the times stay in a column. The date
    // hangs off where the time starts rather than off the end of the weekday,
    // so that the two lines begin together.
    let weekday = middle - WEEKDAY_WIDTH;
    let clock = weekday - CLOCK_WIDTH;

    painter.text(
        egui::pos2(weekday, upper),
        egui::Align2::RIGHT_TOP,
        &reading.time,
        value.clone(),
        foreground,
    );
    painter.text(
        egui::pos2(weekday, upper),
        egui::Align2::LEFT_TOP,
        &format!(", {}", reading.day),
        value.clone(),
        foreground,
    );
    painter.text(
        egui::pos2(clock, lower),
        egui::Align2::LEFT_TOP,
        &reading.date,
        caption.clone(),
        SUBDUED,
    );

    let slot = right - HALF_WIDTH;

    painter.text(
        egui::pos2(slot, upper),
        egui::Align2::RIGHT_TOP,
        &reading.offset,
        value.clone(),
        foreground,
    );

    if reading.half {
        painter.text(
            egui::pos2(slot, upper),
            egui::Align2::LEFT_TOP,
            "½",
            value,
            foreground,
        );
    }
    painter.text(
        egui::pos2(slot, lower),
        egui::Align2::RIGHT_TOP,
        &reading.abbreviation,
        caption,
        SUBDUED,
    );

    response
}

struct Slashtime {
    locations: Vec<Locality>,
    pivot: usize,
    icons: Icons,
    meeting: Option<Meeting>,
    selected: Vec<usize>,
}

impl Slashtime {
    fn new(ctx: &egui::Context, locations: Vec<Locality>, pivot: usize) -> Self {
        install_fonts(ctx, &locations);

        Slashtime {
            locations,
            pivot,
            icons: Icons::load(ctx),
            meeting: None,
            selected: Vec::new(),
        }
    }

    // Which rows are worth writing out: the ones picked out by hand, and
    // failing that the two or three the eye goes to anyway. Leaving the
    // default implicit is what keeps the list unmarked until someone asks for
    // something, and the icon table answers the question rather than a second
    // list of rules that could drift away from it.
    fn marked(&self, reading: &Reading) -> bool {
        if self.selected.is_empty() {
            self.icons.choose(reading).is_some()
        } else {
            reading.is_selected
        }
    }

    // what the list is showing: the meeting while there is one, and otherwise
    // the present
    fn showing(&self, now: UtcDateTime) -> UtcDateTime {
        self.meeting
            .as_ref()
            .and_then(|meeting| meeting.instant(&self.locations[self.pivot]))
            .unwrap_or(now)
    }
}

impl eframe::App for Slashtime {
    // whatever the list does not cover is part of the border, not a backdrop
    fn clear_color(&self, _visuals: &egui::Visuals) -> [f32; 4] {
        egui::Color32::BLACK.to_normalized_gamma_f32()
    }

    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        self.draw(ui);
    }
}

impl Slashtime {
    // The whole of the drawing, separated from the App trait so that it can be
    // driven by a test harness, which has a Ui but no eframe::Frame.
    pub fn draw(&mut self, ui: &mut egui::Ui) {
        let now = UtcDateTime::now().expect("system clock");

        // While a meeting is being planned the list shows that instant rather
        // than the present, and the frame turns red to say so.
        let when = self.showing(now);

        let frame = if self.meeting.is_some() {
            WRONG
        } else {
            egui::Color32::BLACK
        };

        let readings = match read(&self.locations, self.pivot, &when, &self.selected) {
            Ok(readings) => readings,
            Err(e) => {
                ui.label(format!("Unable to read the zone database: {}", e));
                return;
            }
        };

        // M puts the list into meeting mode and takes it out again, Escape
        // only ever leaves, Q gives up altogether.
        let (asked, escaped, quit, entered, copied, all, days, months, minutes) =
            ui.ctx().input(|state| {
                let paced = if state.modifiers.shift { 60 } else { 15 };

                let step = |forward: egui::Key, back: egui::Key, by: i32| {
                    by * (i32::from(state.key_pressed(forward))
                        - i32::from(state.key_pressed(back)))
                };

                (
                    state.key_pressed(egui::Key::M),
                    state.key_pressed(egui::Key::Escape),
                    state.key_pressed(egui::Key::Q),
                    state.key_pressed(egui::Key::Enter),
                    // Ctrl+C never arrives as a key press; egui-winit turns
                    // the chord into this instead, which also covers Cmd+C
                    // and a keyboard with a Copy key of its own.
                    state.events.iter().any(|event| *event == egui::Event::Copy),
                    state.modifiers.command && state.key_pressed(egui::Key::A),
                    step(egui::Key::ArrowRight, egui::Key::ArrowLeft, 1),
                    step(egui::Key::PageDown, egui::Key::PageUp, 1),
                    step(egui::Key::ArrowDown, egui::Key::ArrowUp, paced),
                )
            });

        if quit {
            ui.ctx().send_viewport_cmd(egui::ViewportCommand::Close);
        }

        // the readings borrow the location list, so the new pivot is parked
        // here until the loop is done with it.
        let mut chosen = self.pivot;
        let mut toggled = None;
        let icons = &self.icons;

        egui::Frame::NONE
            .fill(frame)
            .inner_margin(egui::Margin::same(1))
            .show(ui, |ui| {
                ui.spacing_mut().item_spacing = egui::vec2(0.0, 0.0);

                for reading in &readings {
                    let response = row(ui, reading, icons);

                    // double clicking a row measures every offset from there
                    // instead, which is the whole point of the program.
                    if response.double_clicked() {
                        chosen = reading.index;
                    }

                    // A single click adds a city to what Enter prints, or
                    // takes it out again. egui counts the second click of a
                    // double click as a click as well, so re-pivoting toggles
                    // twice and leaves the selection where it was; the row
                    // stays put under the pointer in between because the sort
                    // does not depend on the pivot.
                    if response.clicked() {
                        toggled = Some(reading.index);
                    }
                }
            });

        // Escape is the one key that means never mind: it puts the list back
        // to the present and drops whatever was picked out.
        if escaped {
            self.meeting = None;
            self.selected.clear();
        }

        let moved = chosen != self.pivot;

        self.pivot = chosen;

        // the selection is not the meeting's; a meeting is the same list of
        // cities at another moment, so M only moves the moment
        if asked {
            self.meeting = match self.meeting {
                Some(_) => None,
                None => Meeting::new(&self.locations[self.pivot], &now),
            };
        }

        // a click says which cities are wanted, which is a different thing
        // from the pivot the offsets are measured against
        if let Some(index) = toggled {
            match self.selected.iter().position(|&each| each == index) {
                Some(at) => {
                    self.selected.remove(at);
                }
                None => self.selected.push(index),
            }
        }

        if all {
            self.selected = (0..self.locations.len()).collect();
        }

        // the meeting is a reading on the pivot's clock, so when the pivot
        // moves it has to be read again there, or the moment would shift
        if let Some(meeting) = self.meeting.as_mut() {
            if moved {
                if let Some(quoted) = Meeting::at(&self.locations[self.pivot], &when) {
                    *meeting = quoted;
                }
            }

            if days != 0 {
                meeting.shift_day(days);
            }
            if months != 0 {
                meeting.shift_month(months);
            }
            if minutes != 0 {
                meeting.shift_minute(minutes);
            }
        }

        // Enter writes the marked rows out on the console and Ctrl+C puts the
        // same lines on the clipboard.
        if entered || copied {
            let text = block(&readings, &self.locations[self.pivot], &when, |reading| {
                self.marked(reading)
            });

            if !text.is_empty() {
                if entered {
                    println!("{}", text);
                }
                if copied {
                    ui.ctx().copy_text(text);
                }
            }
        }

        // the readouts only change on the minute, so there is no reason to
        // wake up any more often than that.
        ui.ctx()
            .request_repaint_after(std::time::Duration::from_secs(60 - u64::from(now.second())));
    }
}

// the wanted rows as the command line tool would have written them, in the
// order they are drawn in
fn block<F>(readings: &[Reading], pivot: &Locality, when: &UtcDateTime, wanted: F) -> String
where
    F: Fn(&Reading) -> bool,
{
    readings
        .iter()
        .filter(|reading| wanted(reading))
        .filter_map(|reading| format_line(reading.location, pivot, when).ok())
        .collect::<Vec<String>>()
        .join("\n")
}

// The window icon, for X11 and for anything else that takes the icon from the
// window itself. Wayland does not; there the compositor matches the app id
// below against an installed .desktop file and uses the Icon= named there.
fn marble() -> egui::IconData {
    let decoded = image::load_from_memory(ZULU_PNG)
        .expect("icon should be a readable png")
        .to_rgba8();

    egui::IconData {
        width: decoded.width(),
        height: decoded.height(),
        rgba: decoded.into_raw(),
    }
}

fn main() -> eframe::Result {
    let matches = Command::new("slashtime-gui")
        .version(env!("CARGO_PKG_VERSION"))
        .about("Show the time in various places.")
        .arg(
            Arg::new("places")
                .long("places")
                .value_name("filename")
                .value_parser(value_parser!(PathBuf))
                .action(ArgAction::Set)
                .help("The tzlist file listing the places to show, rather than the one in the slashtime config directory."),
        )
        .arg(
            Arg::new("home")
                .help("The IANA name of a zone in the tzlist to measure offsets from, rather than the machine's own time zone."),
        )
        .get_matches();

    let places = matches.get_one::<PathBuf>("places");
    let locations = slashtime::loading::load_tzlist(places.map(PathBuf::as_path), None)
        .expect("unable to load tzlist");

    // Offsets are measured from the location in the machine's own time zone,
    // unless a zone is named on the command line, in which case they are
    // measured from there instead.
    let pivot = match matches.get_one::<String>("home") {
        Some(name) => locations
            .iter()
            .position(|location| location.iana_zone == *name)
            .unwrap_or_else(|| {
                eprintln!("Zone \"{}\" is not present in your tzlist", name);
                std::process::exit(1);
            }),
        None => find_local(&locations).unwrap_or(0),
    };

    // size the window to the list; there is nothing to scroll if it all fits
    let height = locations.len() as f32 * ROW_HEIGHT + 2.0;

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([WIDTH, height])
            // no decorations: the thin border around the list is the whole
            // frame, and it is the frame that turns red during a meeting.
            .with_decorations(false)
            .with_resizable(false)
            .with_icon(marble())
            .with_app_id(APP_ID),
        ..Default::default()
    };

    eframe::run_native(
        "slashtime",
        options,
        Box::new(|cc| Ok(Box::new(Slashtime::new(&cc.egui_ctx, locations, pivot)))),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use egui_kittest::Harness;

    fn places() -> Vec<Locality> {
        [
            "Australia/Sydney",
            "America/Toronto",
            "Europe/London",
            "UTC",
        ]
        .iter()
        .map(|zone| Locality {
            zone: tz::TimeZone::from_posix_tz(zone).unwrap(),
            iana_zone: zone.to_string(),
            city_name: zone.split('/').next_back().unwrap().to_string(),
            country_name: "Somewhere".to_string(),
            is_zulu: *zone == "UTC",
            is_local: *zone == "Australia/Sydney",
            is_home: false,
        })
        .collect()
    }

    // The harness has a Ui but no window, so the app is built on the first
    // pass, once there is a Context to load the face and the icons from.
    fn new_harness<'a>() -> Harness<'a, Option<Slashtime>> {
        let places = places();

        let mut harness = Harness::builder()
            .with_size(egui::vec2(WIDTH + 16.0, 700.0))
            // a double click needs both releases inside egui's 0.3s window,
            // and the harness steps a quarter of a second at a time by default
            .with_step_dt(1.0 / 60.0)
            .build_ui_state(
                move |ui, state: &mut Option<Slashtime>| {
                    state
                        .get_or_insert_with(|| {
                            Slashtime::new(ui.ctx(), places.clone(), find_local(&places).unwrap())
                        })
                        .draw(ui);
                },
                None,
            );

        harness.run();
        harness
    }

    fn app<'h>(harness: &'h Harness<'_, Option<Slashtime>>) -> &'h Slashtime {
        harness.state().as_ref().unwrap()
    }

    // Where on screen a given location has been drawn. The list sorts itself
    // by the time of day, so this moves about with the clock, and again once
    // a meeting pins the list to a moment other than the present.
    fn row_of(harness: &Harness<'_, Option<Slashtime>>, index: usize) -> f32 {
        let slashtime = app(harness);
        let when = slashtime.showing(UtcDateTime::now().unwrap());

        let readings = read(&slashtime.locations, slashtime.pivot, &when, &[]).unwrap();

        let row = readings
            .iter()
            .position(|reading| reading.index == index)
            .unwrap();

        1.0 + row as f32 * ROW_HEIGHT + ROW_HEIGHT / 2.0
    }

    // press and release over a row, each in its own pass, as a mouse does
    fn click_at(harness: &mut Harness<'_, Option<Slashtime>>, y: f32) {
        let pos = egui::pos2(100.0, y);

        for pressed in [true, false] {
            harness.event(egui::Event::PointerButton {
                pos,
                button: egui::PointerButton::Primary,
                pressed,
                modifiers: egui::Modifiers::NONE,
            });
        }

        harness.run();
    }

    // The layout constants were measured off the embedded face, so measure
    // them again: a new face, a new size, or egui changing how it lays text
    // out will show up here rather than as a column quietly out of line.
    #[test]
    fn the_constants_still_match_the_face() {
        let harness = new_harness();

        let value = egui::FontId::proportional(VALUE_SIZE);
        let caption = egui::FontId::proportional(CAPTION_SIZE);

        harness.ctx.fonts_mut(|fonts| {
            let mut width = |text: &str| {
                fonts
                    .layout_no_wrap(text.to_string(), value.clone(), PLAIN)
                    .size()
                    .x
            };

            let widest = (0..7)
                .map(|day| width(&format!(", {}", format_day(day))))
                .fold(0.0, f32::max);

            assert_eq!(widest, WEEKDAY_WIDTH, "the widest weekday");
            assert_eq!(width("00:00"), CLOCK_WIDTH, "the clock");
            assert_eq!(width("½"), HALF_WIDTH, "the ½");

            assert_eq!(fonts.row_height(&value), VALUE_LINE, "the value line");
            assert_eq!(fonts.row_height(&caption), CAPTION_LINE, "the caption line");
        });
    }

    #[test]
    fn m_toggles_planning_and_escape_leaves_it() {
        let mut harness = new_harness();
        assert!(app(&harness).meeting.is_none());

        harness.key_press(egui::Key::M);
        harness.run();
        assert!(app(&harness).meeting.is_some(), "M did not open it");
        assert!(
            app(&harness).selected.is_empty(),
            "M helped itself to a selection"
        );

        harness.key_press(egui::Key::M);
        harness.run();
        assert!(app(&harness).meeting.is_none(), "M did not close it again");

        // Escape means never mind: the moment and the selection both go
        harness.key_press(egui::Key::M);
        harness.run();

        let london = row_of(&harness, 2);

        click_at(&mut harness, london);
        assert_eq!(app(&harness).selected, vec![2], "London did not join");

        harness.key_press(egui::Key::Escape);
        harness.run();
        assert!(app(&harness).meeting.is_none(), "Escape did not close it");
        assert!(
            app(&harness).selected.is_empty(),
            "the selection survived Escape"
        );
    }

    fn planned(harness: &Harness<'_, Option<Slashtime>>) -> (i32, u8, u8, u8, u8) {
        let meeting = app(harness).meeting.as_ref().unwrap();

        (
            meeting.year,
            meeting.month,
            meeting.day,
            meeting.hour,
            meeting.minute,
        )
    }

    #[test]
    fn the_arrow_keys_move_the_planned_moment() {
        let mut harness = new_harness();

        harness.key_press(egui::Key::M);
        harness.run();

        let (year, month, day, hour, _) = planned(&harness);

        harness.key_press(egui::Key::ArrowDown);
        harness.run();
        assert_eq!(planned(&harness).4, 15, "down did not add a quarter hour");

        harness.key_press(egui::Key::ArrowUp);
        harness.run();
        assert_eq!(planned(&harness), (year, month, day, hour, 0));

        harness.key_press(egui::Key::ArrowRight);
        harness.run();
        assert_eq!(planned(&harness).2, day % 28 + 1, "right did not add a day");
    }

    // page keys step a month, not a week
    #[test]
    fn the_page_keys_move_a_month() {
        let mut harness = new_harness();

        harness.key_press(egui::Key::M);
        harness.run();

        let (year, month, ..) = planned(&harness);

        harness.key_press(egui::Key::PageDown);
        harness.run();

        let (after, next, ..) = planned(&harness);
        let (expected_year, expected_month) = step_month(year, month, 1);

        assert_eq!((after, next), (expected_year, expected_month));
    }

    // a quarter hour before midnight, plus a quarter hour, is the next day
    #[test]
    fn minutes_roll_over_into_the_next_day() {
        let mut meeting = Meeting {
            year: 2026,
            month: 12,
            day: 31,
            hour: 23,
            minute: 45,
        };

        meeting.shift_minute(15);

        assert_eq!(
            (meeting.year, meeting.month, meeting.day, meeting.hour),
            (2027, 1, 1, 0)
        );
    }

    // and a day either side of a month's end lands in the right month
    #[test]
    fn days_step_across_the_ends_of_months() {
        let mut meeting = Meeting {
            year: 2026,
            month: 3,
            day: 1,
            hour: 9,
            minute: 0,
        };

        meeting.shift_day(-1);
        assert_eq!((meeting.month, meeting.day), (2, 28));

        meeting.shift_day(1);
        assert_eq!((meeting.month, meeting.day), (3, 1));
    }

    // stepping into a shorter month has to pull the day back
    #[test]
    fn a_month_step_keeps_the_date_real() {
        let mut meeting = Meeting {
            year: 2026,
            month: 1,
            day: 31,
            hour: 9,
            minute: 0,
        };

        meeting.shift_month(1);

        assert_eq!((meeting.month, meeting.day), (2, 28));
    }

    // re-reading the meeting on another clock is a change of how it is said,
    // not of when it is
    #[test]
    fn requoting_the_meeting_keeps_the_instant() {
        let places = places();

        let meeting = Meeting {
            year: 2026,
            month: 9,
            day: 20,
            hour: 21,
            minute: 30,
        };

        let when = meeting.instant(&places[0]).unwrap();
        let quoted = Meeting::at(&places[2], &when).unwrap();

        assert_eq!((quoted.hour, quoted.minute), (12, 30), "London reads 12:30");
        assert_eq!(
            quoted.instant(&places[2]).unwrap(),
            when,
            "the moment moved"
        );
    }

    // what Enter would write out, as the app decides it
    fn marked(harness: &Harness<'_, Option<Slashtime>>) -> String {
        let slashtime = app(harness);
        let when = slashtime.showing(UtcDateTime::now().unwrap());
        let readings = read(&slashtime.locations, slashtime.pivot, &when, &[]).unwrap();

        block(
            &readings,
            &slashtime.locations[slashtime.pivot],
            &when,
            |reading| slashtime.marked(reading),
        )
    }

    // outside a meeting the marked rows are the two or three the icons
    // point at: the pivot, the location in the machine's own time zone, and
    // Zulu
    #[test]
    fn the_marked_rows_are_the_ones_wearing_an_icon() {
        let mut harness = new_harness();

        let text = marked(&harness);
        let lines: Vec<&str> = text.lines().collect();

        assert_eq!(lines.len(), 2, "not Sydney and Zulu alone: {}", text);
        assert!(
            lines.iter().any(|line| line.starts_with("UTC,")),
            "Zulu is missing: {}",
            text
        );
        assert!(
            lines.iter().any(|line| line.starts_with("Sydney,")),
            "Sydney is missing: {}",
            text
        );

        // measuring from somewhere else leaves the house behind, and that is
        // the third row
        let toronto = row_of(&harness, 1);

        click_at(&mut harness, toronto);
        click_at(&mut harness, toronto);

        let text = marked(&harness);

        assert_eq!(text.lines().count(), 3, "Toronto did not join: {}", text);
        assert!(
            text.lines().any(|line| line.starts_with("Toronto,")),
            "Toronto is missing: {}",
            text
        );
    }

    // the pivot wears the marker for the clock everything is measured from,
    // and the row the machine belongs to keeps the house
    #[test]
    fn the_local_marker_follows_the_pivot() {
        let mut harness = new_harness();

        let toronto = row_of(&harness, 1);

        click_at(&mut harness, toronto);
        click_at(&mut harness, toronto);

        let slashtime = app(&harness);
        assert_eq!(slashtime.pivot, 1, "the pivot did not move");

        let when = slashtime.showing(UtcDateTime::now().unwrap());
        let readings = read(&slashtime.locations, slashtime.pivot, &when, &[]).unwrap();

        let marker = |index: usize| {
            readings
                .iter()
                .find(|reading| reading.index == index)
                .and_then(|reading| slashtime.icons.choose(reading))
                .map(|texture| texture.id())
        };

        assert_eq!(
            marker(1),
            Some(slashtime.icons.local.id()),
            "Toronto did not take the marker"
        );
        assert_eq!(
            marker(0),
            Some(slashtime.icons.home.id()),
            "Sydney was left without the house"
        );
    }

    // clicking a row says which cities are in the meeting
    #[test]
    fn only_a_click_changes_the_selection() {
        let mut harness = new_harness();

        harness.key_press(egui::Key::M);
        harness.run();

        let before = app(&harness).selected.clone();

        // sweep the pointer down the whole list
        for step in 0..8 {
            harness.hover_at(egui::pos2(100.0, 10.0 + step as f32 * ROW_HEIGHT));
            harness.run();
        }

        assert_eq!(
            app(&harness).selected,
            before,
            "the selection followed the pointer"
        );
    }

    #[test]
    fn a_click_takes_a_city_in_and_out_again() {
        let mut harness = new_harness();

        harness.key_press(egui::Key::M);
        harness.run();

        // London, which is not the pivot the mode started with
        let london = row_of(&harness, 2);

        click_at(&mut harness, london);
        assert_eq!(app(&harness).selected, vec![2], "London did not join");

        click_at(&mut harness, london);
        assert!(app(&harness).selected.is_empty(), "London did not leave");
    }

    // the second click of a double click is a click as well, so the two
    // cancel and a re-pivot leaves the selection alone
    #[test]
    fn a_double_click_moves_the_pivot_and_nothing_else() {
        let mut harness = new_harness();

        harness.key_press(egui::Key::M);
        harness.run();

        // taking the whole list in by keyboard, so that the only clicks in
        // this test are the two being measured
        harness.key_press_modifiers(egui::Modifiers::COMMAND, egui::Key::A);
        harness.run();

        let toronto = row_of(&harness, 1);

        click_at(&mut harness, toronto);
        click_at(&mut harness, toronto);

        let mut after = app(&harness).selected.clone();
        after.sort();

        assert_eq!(app(&harness).pivot, 1, "the pivot did not move");
        assert_eq!(after, vec![0, 1, 2, 3], "the selection moved");
    }

    #[test]
    fn ctrl_a_takes_in_every_city() {
        let mut harness = new_harness();

        // no meeting needed; the selection is the list's, not the mode's
        harness.key_press_modifiers(egui::Modifiers::COMMAND, egui::Key::A);
        harness.run();

        assert_eq!(app(&harness).selected, vec![0, 1, 2, 3]);

        harness.key_press(egui::Key::Escape);
        harness.run();

        assert!(app(&harness).selected.is_empty(), "Escape left some behind");
    }

    // Asking to copy earns a repaint, and a whole run would draw again and
    // leave the command behind, so the answer is read off the single pass
    // that took the event.
    fn copied(harness: &mut Harness<'_, Option<Slashtime>>) -> Option<String> {
        harness.event(egui::Event::Copy);
        harness.step();

        harness
            .output()
            .platform_output
            .commands
            .iter()
            .find_map(|command| match command {
                egui::OutputCommand::CopyText(text) => Some(text.clone()),
                _ => None,
            })
    }

    #[test]
    fn copy_takes_the_same_lines_enter_prints() {
        let mut harness = new_harness();

        // with nothing picked out it is the marked rows, as on the console
        let text = copied(&mut harness).expect("nothing was copied");

        assert_eq!(
            text,
            marked(&harness),
            "the clipboard and the console differ"
        );
        assert_eq!(
            text.lines().count(),
            2,
            "not Sydney and Zulu alone: {}",
            text
        );

        let london = row_of(&harness, 2);

        click_at(&mut harness, london);

        let text = copied(&mut harness).expect("nothing was copied");

        assert_eq!(text.lines().count(), 1, "not London alone: {}", text);
        assert!(text.starts_with("London,"), "London is missing: {}", text);
    }

    // re-pivoting reads the meeting off the new clock at the time it is
    // already showing, so the list does not move
    #[test]
    fn a_re_pivot_keeps_the_planned_instant() {
        let mut harness = new_harness();

        harness.key_press(egui::Key::M);
        harness.run();

        let slashtime = app(&harness);
        let before = slashtime
            .meeting
            .as_ref()
            .unwrap()
            .instant(&slashtime.locations[slashtime.pivot])
            .unwrap();

        let toronto = row_of(&harness, 1);

        click_at(&mut harness, toronto);
        click_at(&mut harness, toronto);

        let slashtime = app(&harness);
        let after = slashtime
            .meeting
            .as_ref()
            .unwrap()
            .instant(&slashtime.locations[slashtime.pivot])
            .unwrap();

        assert_eq!(slashtime.pivot, 1, "the pivot did not move");
        assert_eq!(after, before, "the planned moment moved");
    }
}

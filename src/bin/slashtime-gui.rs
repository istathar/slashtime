use eframe::egui;
use slashtime::{
    days_in_month, find_local, format_date, format_offset_parts, format_time, week_day, Band,
    Locality,
};
use std::path::{Path, PathBuf};
use tz::{TzError, UtcDateTime};

// the palette carried over from the java-gnome original. The list deliberately
// ignores the desktop theme; here the shading is data, not chrome.
const WORK: egui::Color32 = egui::Color32::from_rgb(0xff, 0xff, 0xff);
const CIVIL: egui::Color32 = egui::Color32::from_rgb(0xdd, 0xdd, 0xdd);
const NIGHT: egui::Color32 = egui::Color32::from_rgb(0x77, 0x77, 0x77);

const SUBDUED: egui::Color32 = egui::Color32::from_rgb(0xa1, 0xa1, 0xa1);
const PLAIN: egui::Color32 = egui::Color32::from_rgb(0x00, 0x00, 0x00);
const LOCAL: egui::Color32 = egui::Color32::from_rgb(0x00, 0x00, 0xff);
const ZULU: egui::Color32 = egui::Color32::from_rgb(0x2f, 0xb9, 0x25);
const LOCAL_DARK: egui::Color32 = egui::Color32::from_rgb(0x32, 0xfd, 0xff);
const ZULU_DARK: egui::Color32 = egui::Color32::from_rgb(0xa0, 0xff, 0x97);
const HOVER: egui::Color32 = egui::Color32::from_rgb(0x35, 0x84, 0xe4);

// the frame turns red while a meeting time is being planned, as the original
// did, so the list is never mistaken for the actual time somewhere
const WRONG: egui::Color32 = egui::Color32::from_rgb(0xd0, 0x18, 0x18);

// the planner is a dark panel, as the original's was against the light list
const BACKDROP: egui::Color32 = egui::Color32::from_rgb(0x30, 0x30, 0x30);

// room set aside beside a slider for the reading it carries
const SLIDER_READOUT: f32 = 52.0;

// the knob of a slider, and the rail it runs along
const GRIP: egui::Color32 = egui::Color32::from_rgb(0x96, 0x96, 0x96);

// This has to match the basename of the installed .desktop file, which is how
// a wayland compositor works out which icon belongs to the window; there is no
// other route, as wayland ignores icons set on the window itself.
//
// Note that eframe only sends the app id at all when built with its "wayland"
// feature, which Cargo.toml enables explicitly. Drop that feature and this
// becomes a silent no-op rather than a compile error.
const APP_ID: &str = "org.aesiniath.Slashtime";

// the original window was about seven times as tall as it was wide with a
// list this long, and fifteen years of muscle memory is worth honouring.
const WIDTH: f32 = 272.0;

// the planner opens beside the list rather than in a window of its own, so
// that every zone stays visible while the time is being moved around
const PLANNER_WIDTH: f32 = 268.0;
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

// Take the slack the face already carries out of each gap, so that what is
// left is the clear space actually asked for above.
const LEAD: f32 = PADDING - VALUE_ABOVE;
const TO_CAPTION: f32 = VALUE_LINE - VALUE_BELOW + SEPARATION - CAPTION_ABOVE;
const TRAIL: f32 = PADDING - CAPTION_BELOW;
const ROW_HEIGHT: f32 = LEAD + TO_CAPTION + CAPTION_LINE + TRAIL;

// the icon sits in a reserved column at the left, so that the city names line
// up whether or not a given row has one.
const ICON_COLUMN: f32 = 34.0;
const ICON_SIZE: f32 = 20.0;

// width set aside at the right hand end for the offset and the zone code,
// which the time and date are then right aligned against.
const OFFSET_COLUMN: f32 = 50.0;

const MONTHS: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];

const DAYS: [&str; 7] = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];

// The original asked for "DejaVu Sans, 11", which is no longer installed
// anywhere by default. Noto Sans stands in for it: what matters is that its
// numerals are all one width, so the clock does not shuffle sideways as the
// minutes turn over, without it being a teletype monospace.
//
// It is embedded rather than looked for on the machine. Every face has its own
// line heights, so taking whatever happened to be installed would mean
// measuring at startup and laying out differently from one machine to the
// next; with the face fixed, the vertical layout above is a constant. Pinned
// to Regular and subset to the Latin a tzlist can hold, it costs 29kB rather
// than the 712kB of the full variable font. See share/fonts/OFL.txt.
const FACE: &[u8] = include_bytes!("../../share/fonts/NotoSans-Regular-subset.ttf");

// Put the face at the head of the family, leaving egui's built in fonts behind
// it to cover anything it is missing.
fn install_fonts(ctx: &egui::Context) {
    let mut fonts = egui::FontDefinitions::default();

    fonts.font_data.insert(
        "sans".to_string(),
        std::sync::Arc::new(egui::FontData::from_static(FACE)),
    );
    fonts
        .families
        .entry(egui::FontFamily::Proportional)
        .or_default()
        .insert(0, "sans".to_string());

    ctx.set_fonts(fonts);
}

// Pointing at a widget must change its colour and nothing else. Out of the box
// egui gives an inactive button no border at all and a hovered one a border a
// pixel wide, with a bigger corner radius and a heavier glyph; since a border
// is drawn centred on the edge it spills half a pixel outside, and the button
// appears to swell and shift under the pointer.
fn steady_widgets(ctx: &egui::Context) {
    ctx.all_styles_mut(|style| {
        let widgets = &mut style.visuals.widgets;
        let (radius, stroke) = (
            widgets.inactive.corner_radius,
            widgets.inactive.fg_stroke.width,
        );

        for state in [&mut widgets.hovered, &mut widgets.active, &mut widgets.open] {
            state.bg_stroke.width = 0.0;
            state.corner_radius = radius;
            state.fg_stroke.width = stroke;
        }

        // A slider draws its rail and its handle in the same colour, so at rest
        // the handle is nothing but an outline and does not look like anything
        // you could take hold of. A filled knob on a thin track is
        // unmistakable, and the trailing fill says where the value sits.
        widgets.inactive.bg_fill = GRIP;
        widgets.hovered.bg_fill = GRIP;
        widgets.active.bg_fill = GRIP;

        style.visuals.handle_shape = egui::style::HandleShape::Circle;
        style.visuals.slider_trailing_fill = true;
        style.spacing.slider_rail_height = 4.0;
    });
}

// the icons are embedded rather than read from disk; they are tiny, and this
// saves the program having to work out where it was installed.
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

    // which marker this location gets, in the order the original tested them:
    // where you are beats where you live, which beats Zulu.
    fn choose(&self, location: &Locality) -> Option<&egui::TextureHandle> {
        if location.is_local {
            Some(&self.local)
        } else if location.is_home {
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

// A meeting is a wall clock reading somewhere: name the place and the time you
// want it to be there, and the instant that turns out to be is what the whole
// list is then shown at.
struct Meeting {
    place: usize,
    year: i32,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
}

impl Meeting {
    // start from the hour just gone where the meeting is, as the original did
    fn new(locations: &[Locality], place: usize, now: &UtcDateTime) -> Option<Self> {
        let there = now.project(locations[place].zone.as_ref()).ok()?;

        Some(Meeting {
            place,
            year: there.year(),
            month: there.month(),
            day: there.month_day(),
            hour: there.hour(),
            minute: 0,
        })
    }

    fn instant(&self, locations: &[Locality]) -> Option<UtcDateTime> {
        locations[self.place]
            .instant(self.year, self.month, self.day, self.hour, self.minute)
            .ok()
            .flatten()
    }

    // moving between months has to pull the day back when the new month is
    // shorter, or the date would not exist
    fn shift_month(&mut self, by: i32) {
        let months = self.year * 12 + i32::from(self.month) - 1 + by;

        self.year = months.div_euclid(12);
        self.month = (months.rem_euclid(12) + 1) as u8;
        self.day = self.day.min(days_in_month(self.year, self.month));
    }
}

// one location as it appears at a given moment, relative to a given pivot.
// All of it is derived, so it is recomputed each pass rather than cached and
// invalidated.
struct Reading<'a> {
    index: usize,
    location: &'a Locality,
    time: String,
    date: String,
    offset: String,
    half: bool,
    abbreviation: String,
    band: Band,
    key: u8,
}

fn read<'a>(
    locations: &'a [Locality],
    pivot: &Locality,
    when: &UtcDateTime,
) -> Result<Vec<Reading<'a>>, TzError> {
    let mut readings = Vec::with_capacity(locations.len());

    for (index, location) in locations.iter().enumerate() {
        let there = when.project(location.zone.as_ref())?;
        let offset = format_offset_parts(location.offset(when)? - pivot.offset(when)?);

        readings.push(Reading {
            index,
            location,
            time: format_time(&there),
            date: format_date(&there),
            offset: offset.0,
            half: offset.1,
            abbreviation: location.abbreviation(when)?,
            band: location.band(when)?,
            key: location.sort_key(when)?,
        });
    }

    readings.sort_by_key(|reading| reading.key);

    Ok(readings)
}

// background comes from the hour, foreground from which location this is.
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

// Each row is painted into an exact rectangle rather than laid out from its
// contents, so that the shaded bands line up and reach both edges regardless
// of how long a city name happens to be.
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

    if let Some(icon) = icons.choose(reading.location) {
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

    painter.text(
        egui::pos2(middle, upper),
        egui::Align2::RIGHT_TOP,
        &reading.time,
        value.clone(),
        foreground,
    );
    painter.text(
        egui::pos2(middle, lower),
        egui::Align2::RIGHT_TOP,
        &reading.date,
        caption.clone(),
        SUBDUED,
    );

    // The ½ gets a slot of its own whether or not this zone has one, so that
    // the units column stays put all the way down the list.
    let slot = painter
        .layout_no_wrap("½".to_string(), value.clone(), foreground)
        .size()
        .x;

    painter.text(
        egui::pos2(right - slot, upper),
        egui::Align2::RIGHT_TOP,
        &reading.offset,
        value.clone(),
        foreground,
    );

    if reading.half {
        painter.text(
            egui::pos2(right - slot, upper),
            egui::Align2::LEFT_TOP,
            "½",
            value,
            foreground,
        );
    }
    painter.text(
        egui::pos2(right - slot, lower),
        egui::Align2::RIGHT_TOP,
        &reading.abbreviation,
        caption,
        SUBDUED,
    );

    // the original highlighted whichever row the pointer was over, and
    // dropped the highlight again on the way out.
    if response.hovered() {
        painter.rect_stroke(
            rect,
            0.0,
            egui::Stroke::new(1.0, HOVER),
            egui::StrokeKind::Inside,
        );
    }

    response
}

// A month laid out as the original's calendar was, with the weeks running
// Sunday to Saturday and the chosen day marked.
fn calendar(ui: &mut egui::Ui, meeting: &mut Meeting) {
    ui.horizontal(|ui| {
        if ui.small_button("\u{2039}").clicked() {
            meeting.shift_month(-1);
        }

        // the far arrow is placed first so that the month and year can have
        // all the room between the two, and sit centred in it
        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if ui.small_button("\u{203a}").clicked() {
                meeting.shift_month(1);
            }

            ui.add_sized(
                ui.available_size(),
                egui::Label::new(format!(
                    "{} {}",
                    MONTHS[usize::from(meeting.month) - 1],
                    meeting.year
                )),
            );
        });
    });

    ui.add_space(2.0);

    // The width of a day is worked out from the room there is, rather than
    // asked for: a Grid sizes its columns to suit itself and pushed Saturday
    // off the end of the panel.
    ui.spacing_mut().item_spacing = egui::vec2(1.0, 1.0);
    ui.spacing_mut().button_padding = egui::vec2(1.0, 1.0);

    let cell = (ui.available_width() - 8.0) / 7.0;

    let first = week_day(meeting.year, meeting.month, 1).unwrap_or(0);
    let days = days_in_month(meeting.year, meeting.month);

    ui.horizontal(|ui| {
        for name in ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"] {
            ui.add_sized(
                [cell, 12.0],
                egui::Label::new(egui::RichText::new(name).size(10.0).color(SUBDUED)),
            );
        }
    });

    let mut day = 1u8;

    for week in 0..6 {
        if day > days {
            break;
        }

        ui.horizontal(|ui| {
            for column in 0..7 {
                if (week == 0 && column < first) || day > days {
                    ui.add_sized([cell, 16.0], egui::Label::new(""));
                    continue;
                }

                if ui
                    .add_sized(
                        [cell, 16.0],
                        // selectable() frames the day only when it is the
                        // chosen one, so the selection shows without the
                        // pointer having to be over it
                        egui::Button::selectable(day == meeting.day, format!("{}", day)),
                    )
                    .clicked()
                {
                    meeting.day = day;
                }

                day += 1;
            }
        });
    }
}

// The planner proper. It says where and when, and everything it changes is
// reflected in the list behind it, which is showing that instant rather than
// the present.
fn planner(ui: &mut egui::Ui, meeting: &mut Meeting, locations: &[Locality]) -> bool {
    let place = &locations[meeting.place];
    let mut done = false;

    ui.horizontal(|ui| {
        ui.label(egui::RichText::new("Find a meeting time").strong());

        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if ui.button("Done").clicked() {
                done = true;
            }
        });
    });

    ui.separator();
    ui.add_space(4.0);
    ui.label("Set the time at:");

    ui.vertical_centered(|ui| {
        ui.label(egui::RichText::new(&place.city_name).size(22.0));
        ui.label(&place.country_name);
    });

    ui.add_space(2.0);
    ui.label("to:");

    ui.vertical_centered(|ui| {
        ui.label(
            egui::RichText::new(format!("{:02}:{:02}", meeting.hour, meeting.minute))
                .size(18.0)
                .strong(),
        );
        ui.label(format!(
            "{}, {} {} {:02}",
            DAYS[usize::from(week_day(meeting.year, meeting.month, meeting.day).unwrap_or(0))],
            meeting.day,
            &MONTHS[usize::from(meeting.month) - 1][..3],
            meeting.year % 100
        ));
    });

    ui.add_space(4.0);
    ui.vertical_centered(|ui| {
        ui.label(egui::RichText::new("Click a city to move the meeting there").italics());
    });

    ui.add_space(6.0);
    ui.separator();
    ui.add_space(6.0);

    calendar(ui, meeting);

    ui.add_space(8.0);
    // The rail is given whatever is left once the readout beside it has been
    // allowed for, so the pair span the panel. Both are told the same width
    // rather than sizing to their own contents, or the two rails would end at
    // different places as the readings change.
    ui.spacing_mut().slider_width = ui.available_width() - SLIDER_READOUT;

    ui.add(egui::Slider::new(&mut meeting.hour, 0..=23).suffix("h"));
    ui.add(
        egui::Slider::new(&mut meeting.minute, 0..=45)
            .step_by(15.0)
            .suffix("m"),
    );

    done
}

// Screenshots are written as a PPM, which needs no encoder, and converted
// elsewhere if a real image format is wanted.
fn write_ppm(path: &Path, image: &egui::ColorImage) -> std::io::Result<()> {
    let [width, height] = image.size;

    let mut out = Vec::with_capacity(width * height * 3 + 20);
    out.extend_from_slice(format!("P6\n{} {}\n255\n", width, height).as_bytes());

    for pixel in &image.pixels {
        out.extend_from_slice(&[pixel.r(), pixel.g(), pixel.b()]);
    }

    std::fs::write(path, out)
}

struct Slashtime {
    locations: Vec<Locality>,
    pivot: usize,
    icons: Icons,
    meeting: Option<Meeting>,
    width: Option<f32>,
    capture: Option<PathBuf>,
    passes: u32,
}

impl Slashtime {
    fn new(ctx: &egui::Context, locations: Vec<Locality>) -> Self {
        install_fonts(ctx);
        steady_widgets(ctx);

        let pivot = find_local(&locations).unwrap_or(0);

        Slashtime {
            locations,
            pivot,
            icons: Icons::load(ctx),
            meeting: None,
            width: None,
            capture: std::env::var_os("SLASHTIME_SCREENSHOT").map(PathBuf::from),
            passes: 0,
        }
    }

    // when SLASHTIME_SCREENSHOT names a file, draw a couple of passes to let
    // the layout settle, ask for the window contents, write them out, and quit.
    fn capture(&mut self, ctx: &egui::Context) {
        let Some(path) = &self.capture else {
            return;
        };

        self.passes += 1;

        if self.passes == 2 {
            ctx.send_viewport_cmd(egui::ViewportCommand::Screenshot(egui::UserData::default()));
        }

        let image = ctx.input(|state| {
            state.events.iter().find_map(|event| match event {
                egui::Event::Screenshot { image, .. } => Some(image.clone()),
                _ => None,
            })
        });

        if let Some(image) = image {
            write_ppm(path, &image).expect("write screenshot");
            ctx.send_viewport_cmd(egui::ViewportCommand::Close);
        }

        ctx.request_repaint();
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
        let when = self
            .meeting
            .as_ref()
            .and_then(|meeting| meeting.instant(&self.locations))
            .unwrap_or(now);

        let frame = if self.meeting.is_some() {
            WRONG
        } else {
            egui::Color32::BLACK
        };

        let readings = match read(&self.locations, &self.locations[self.pivot], &when) {
            Ok(readings) => readings,
            Err(e) => {
                ui.label(format!("Unable to read the zone database: {}", e));
                return;
            }
        };

        // the readings borrow the location list, so the new pivot is parked
        // here until the loop is done with it.
        // M shows the planner and hides it again, Escape only ever hides it,
        // Q gives up altogether.
        let (asked, escaped, quit) = ui.ctx().input(|state| {
            (
                state.key_pressed(egui::Key::M),
                state.key_pressed(egui::Key::Escape),
                state.key_pressed(egui::Key::Q),
            )
        });

        if quit {
            ui.ctx().send_viewport_cmd(egui::ViewportCommand::Close);
        }

        let mut chosen = self.pivot;
        let mut target = None;
        let icons = &self.icons;
        let planning = self.meeting.is_some();

        let mut shut = false;

        ui.horizontal_top(|ui| {
            ui.spacing_mut().item_spacing = egui::vec2(0.0, 0.0);

            let tall = ui.available_height();

            // Each column has to be told to lay itself out downwards: inside a
            // horizontal parent the children inherit its direction, and the
            // rows would otherwise be dealt out sideways.
            ui.allocate_ui_with_layout(
                egui::vec2(WIDTH, tall),
                egui::Layout::top_down(egui::Align::Min),
                |ui| {
                    egui::Frame::NONE
                        .fill(frame)
                        .inner_margin(egui::Margin::same(1))
                        .show(ui, |ui| {
                            ui.set_min_width(WIDTH - 2.0);
                            ui.spacing_mut().item_spacing = egui::vec2(0.0, 0.0);

                            for reading in &readings {
                                let response = row(ui, reading, icons);

                                // double clicking a row measures every offset from there
                                // instead, which is the whole point of the program.
                                if response.double_clicked() {
                                    chosen = reading.index;
                                }

                                // where the meeting is, on the other hand, is chosen by a
                                // single click; the two are deliberately separate.
                                if planning && response.clicked() {
                                    target = Some(reading.index);
                                }
                            }
                        });
                },
            );

            if let Some(meeting) = self.meeting.as_mut() {
                ui.allocate_ui_with_layout(
                    egui::vec2(PLANNER_WIDTH, tall),
                    egui::Layout::top_down(egui::Align::Min),
                    |ui| {
                        egui::Frame::NONE
                            .fill(BACKDROP)
                            .inner_margin(egui::Margin::same(8))
                            .show(ui, |ui| {
                                ui.set_min_width(PLANNER_WIDTH - 16.0);
                                ui.set_min_height(tall - 16.0);
                                shut = planner(ui, meeting, &self.locations);
                            });
                    },
                );
            }
        });

        if shut || escaped {
            self.meeting = None;
        }

        self.pivot = chosen;

        if asked {
            self.meeting = match self.meeting {
                Some(_) => None,
                None => Meeting::new(&self.locations, self.pivot, &now),
            };
        }

        // hovering a row moves the meeting there, which is a different thing
        // from the pivot the offsets are measured against
        if let (Some(meeting), Some(index)) = (self.meeting.as_mut(), target) {
            meeting.place = index;
        }

        // The window is only as wide as what it is showing. Ask when the
        // answer changes, never because the surface came back a fraction
        // different from what was asked for: comparing the two means asking
        // again on every repaint, and the window crawls across the desk
        // whenever anything is hovered.
        let wanted = if self.meeting.is_some() {
            WIDTH + PLANNER_WIDTH
        } else {
            WIDTH
        };

        if self.width != Some(wanted) {
            let height = self.locations.len() as f32 * ROW_HEIGHT + 2.0;

            ui.ctx()
                .send_viewport_cmd(egui::ViewportCommand::InnerSize(egui::vec2(wanted, height)));
            self.width = Some(wanted);
        }

        self.capture(ui.ctx());

        // the readouts only change on the minute, so there is no reason to
        // wake up any more often than that.
        ui.ctx()
            .request_repaint_after(std::time::Duration::from_secs(60 - u64::from(now.second())));
    }
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
    let locations = slashtime::loading::load_tzlist(None).expect("unable to load tzlist");

    // size the window to the list; there is nothing to scroll if it all fits
    let height = locations.len() as f32 * ROW_HEIGHT + 2.0;

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([WIDTH, height])
            // as the original did: no decorations, the thin black border
            // around the list is the whole frame.
            .with_decorations(false)
            .with_resizable(false)
            .with_icon(marble())
            .with_app_id(APP_ID),
        ..Default::default()
    };

    eframe::run_native(
        "slashtime",
        options,
        Box::new(|cc| Ok(Box::new(Slashtime::new(&cc.egui_ctx, locations)))),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use egui_kittest::kittest::Queryable as _;
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
    fn harness<'a>() -> Harness<'a, Option<Slashtime>> {
        let places = places();

        let size = egui::vec2(WIDTH + PLANNER_WIDTH + 16.0, 700.0);

        let mut harness = Harness::builder().with_size(size).build_ui_state(
            move |ui, state: &mut Option<Slashtime>| {
                // The harness sizes its frame to whatever the app drew last
                // pass, which once the list alone had been drawn left the
                // planner outside the region clicks are accepted in. A real
                // window does not do this, so hold the Ui open.
                ui.set_min_size(size);

                state
                    .get_or_insert_with(|| Slashtime::new(ui.ctx(), places.clone()))
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

    #[test]
    fn m_toggles_the_planner_and_escape_puts_it_away() {
        let mut harness = harness();
        assert!(app(&harness).meeting.is_none());

        harness.key_press(egui::Key::M);
        harness.run();
        assert!(app(&harness).meeting.is_some(), "M did not open it");

        harness.key_press(egui::Key::M);
        harness.run();
        assert!(app(&harness).meeting.is_none(), "M did not close it again");

        harness.key_press(egui::Key::M);
        harness.run();
        harness.key_press(egui::Key::Escape);
        harness.run();
        assert!(app(&harness).meeting.is_none(), "Escape did not close it");
    }

    fn resizes(harness: &Harness<'_, Option<Slashtime>>) -> Vec<egui::Vec2> {
        harness
            .output()
            .viewport_output
            .values()
            .flat_map(|viewport| viewport.commands.iter())
            .filter_map(|command| match command {
                egui::ViewportCommand::InnerSize(size) => Some(*size),
                _ => None,
            })
            .collect()
    }

    // asking to be resized over and over makes the window crawl about
    #[test]
    fn the_window_settles_at_one_size() {
        let mut harness = harness();

        // a surface that disagrees with what was asked for must not provoke
        // another request on every repaint
        harness.set_size(egui::vec2(WIDTH + PLANNER_WIDTH + 7.0, 400.0));
        harness.run();
        harness.run();

        assert!(resizes(&harness).is_empty(), "still asking to be resized");

        harness.key_press(egui::Key::M);
        harness.run();
        harness.run();

        assert!(
            resizes(&harness).is_empty(),
            "still asking to be resized once the planner is open"
        );

        harness.get_by_label("\u{203a}").hover();
        harness.run();

        assert!(
            resizes(&harness).is_empty(),
            "asking to be resized merely because something is hovered"
        );
    }

    #[test]
    fn the_month_sits_centred_between_its_arrows() {
        let mut harness = harness();

        harness.key_press(egui::Key::M);
        harness.run();

        let caption = {
            let meeting = app(&harness).meeting.as_ref().unwrap();

            format!(
                "{} {}",
                MONTHS[usize::from(meeting.month) - 1],
                meeting.year
            )
        };

        let month = harness.get_by_label(&caption).rect();
        let back = harness.get_by_label("\u{2039}").rect();
        let on = harness.get_by_label("\u{203a}").rect();

        let before = month.min.x - back.max.x;
        let after = on.min.x - month.max.x;

        assert!(
            (before - after).abs() < 1.5,
            "month has {} before it and {} after",
            before,
            after
        );
    }

    // both rails must end in the same place whatever the readings say
    #[test]
    fn the_sliders_line_up_with_each_other() {
        let mut harness = harness();

        harness.key_press(egui::Key::M);
        harness.run();

        let caption = {
            let meeting = app(&harness).meeting.as_ref().unwrap();

            format!(
                "{} {}",
                MONTHS[usize::from(meeting.month) - 1],
                meeting.year
            )
        };

        let rails: Vec<_> = harness
            .get_all_by_role(egui::accesskit::Role::Slider)
            .map(|slider| slider.rect())
            .collect();

        assert_eq!(rails.len(), 2, "expected an hour and a minute slider");
        assert!(
            (rails[0].min.x - rails[1].min.x).abs() < 0.5
                && (rails[0].width() - rails[1].width()).abs() < 0.5,
            "rails disagree: {:?} against {:?}",
            rails[0],
            rails[1]
        );

        // and they reach across the panel, not just part of it
        let panel = harness.get_by_label(&caption).rect();
        assert!(
            rails[0].width() > panel.width() * 0.6,
            "rail is only {} of a {} panel",
            rails[0].width(),
            panel.width()
        );
    }

    // pointing at a widget must not change its shape, only its colour
    #[test]
    fn hovering_changes_no_geometry() {
        let harness = harness();

        harness.ctx.all_styles_mut(|style| {
            let widgets = &style.visuals.widgets;

            for (name, state) in [
                ("hovered", &widgets.hovered),
                ("active", &widgets.active),
                ("open", &widgets.open),
            ] {
                assert_eq!(
                    state.bg_stroke.width, widgets.inactive.bg_stroke.width,
                    "{} draws a border the inactive state does not",
                    name
                );
                assert_eq!(
                    state.corner_radius, widgets.inactive.corner_radius,
                    "{} rounds its corners differently",
                    name
                );
                assert_eq!(
                    state.fg_stroke.width, widgets.inactive.fg_stroke.width,
                    "{} strokes its text more heavily",
                    name
                );
            }
        });
    }

    // pointing at a month arrow was shoving the rest of the panel sideways
    #[test]
    fn hovering_the_month_arrow_moves_nothing() {
        let mut harness = harness();

        harness.key_press(egui::Key::M);
        harness.run();

        let before = harness.get_by_label("15").rect();

        harness.get_by_label("\u{203a}").hover();
        harness.run();

        let after = harness.get_by_label("15").rect();

        assert_eq!(
            (before.min.x, before.min.y),
            (after.min.x, after.min.y),
            "the calendar moved when the arrow was pointed at"
        );
    }

    // hovering used to drag the meeting around as the pointer crossed the list
    #[test]
    fn only_a_click_moves_the_meeting() {
        let mut harness = harness();

        harness.key_press(egui::Key::M);
        harness.run();

        let before = app(&harness).meeting.as_ref().unwrap().place;

        // sweep the pointer down the whole list
        for step in 0..8 {
            harness.hover_at(egui::pos2(100.0, 10.0 + step as f32 * ROW_HEIGHT));
            harness.run();
        }

        assert_eq!(
            app(&harness).meeting.as_ref().unwrap().place,
            before,
            "the meeting followed the pointer"
        );
    }
}

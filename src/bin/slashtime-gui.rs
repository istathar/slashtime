use eframe::egui;
use slashtime::{find_local, format_date, format_offset_parts, format_time, Band, Locality};
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
const VALUE_SIZE: f32 = 14.7;
const CAPTION_SIZE: f32 = 9.5;
const EDGE: f32 = 4.0;

// A row is a little space, the city and time line, a little more space, the
// caption line, then a little space again. The height of the row follows from
// those three rather than being a number in its own right: the gap between one
// row and the next is TRAIL + LEAD, so it can be tuned directly instead of
// being whatever slack happened to be left under the caption.
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
    capture: Option<PathBuf>,
    passes: u32,
}

impl Slashtime {
    fn new(ctx: &egui::Context, locations: Vec<Locality>) -> Self {
        install_fonts(ctx);

        let pivot = find_local(&locations).unwrap_or(0);

        Slashtime {
            locations,
            pivot,
            icons: Icons::load(ctx),
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
        let now = UtcDateTime::now().expect("system clock");

        let readings = match read(&self.locations, &self.locations[self.pivot], &now) {
            Ok(readings) => readings,
            Err(e) => {
                ui.label(format!("Unable to read the zone database: {}", e));
                return;
            }
        };

        // the readings borrow the location list, so the new pivot is parked
        // here until the loop is done with it.
        let mut chosen = self.pivot;
        let icons = &self.icons;

        egui::Frame::NONE
            .fill(egui::Color32::BLACK)
            .inner_margin(egui::Margin::same(1))
            .show(ui, |ui| {
                ui.spacing_mut().item_spacing = egui::vec2(0.0, 0.0);

                for reading in &readings {
                    // double clicking a row measures every offset from there
                    // instead, which is the whole point of the program.
                    if row(ui, reading, icons).double_clicked() {
                        chosen = reading.index;
                    }
                }
            });

        self.pivot = chosen;

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

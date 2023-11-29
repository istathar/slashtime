use crossterm::{
    queue,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::{Clear, ClearType},
};
use csv::ReaderBuilder;
use serde::Deserialize;
use std::fs::File;
use std::path::{Path, PathBuf};
use tz::DateTime;
use tz::TimeZone;

fn main() -> Result<(), tz::TzError> {
    let lima = tz::TimeZone::local()?;
    let local_offset = lima.find_current_local_time_type()?.ut_offset();

    let now = tz::UtcDateTime::now()?;

    // Ingest the user's tzinfo file.

    let path = find_tzlist_file()?;
    let places = tzinfo_parser(&path).unwrap();

    // We now set about converting into Localities. First add an entry for
    // UTC, then convert the user supplied places.

    let mut locations = Vec::with_capacity(places.len() + 1);

    locations.push(Locality {
        zone: tz::TimeZone::utc(),
        is_zulu: true,
        is_home: false,
        offset_zulu: 0,
        offset_local: -local_offset,
        city_name: "Zulu".to_string(),
        country_name: "Universal Time".to_string(),
        abbreviation: "UTC".to_string(),
    });

    // Now add an entry for each of the places present in the tzinfo file.

    for place in places {
        let tz = tz::TimeZone::from_posix_tz(&place.iana_zone)?;
        let local = tz.find_current_local_time_type()?;
        let offset = local.ut_offset();
        let code = refine_zone_abbreviation(&place.iana_zone, local.time_zone_designation());

        let home = tz == lima;

        locations.push(Locality {
            zone: tz,
            is_zulu: false,
            is_home: home,
            offset_zulu: offset,
            offset_local: offset - local_offset,
            city_name: place.city_name,
            country_name: place.country_name,
            abbreviation: code,
        });
    }

    // Order the locations by offset.

    locations.sort();

    // Output the formatted locality, time, date, and offest for each location.

    let mut out = std::io::stdout();

    for location in locations {
        let there = now.project(location.zone.as_ref())?;
        if location.is_zulu {
            // using the macro
            queue!(
                out,
                SetForegroundColor(Color::DarkGreen),
                Print(format_line(&location, &there)),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n"),
            )?;
        } else if location.is_home {
            queue!(
                out,
                SetForegroundColor(Color::DarkCyan),
                Print(format_line(&location, &there)),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n")
            )?;
        } else {
            queue!(
                out,
                Print(format_line(&location, &there)),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n"),
            )?;
        }
    }

    Ok(())
}

// a struct for the IANA zone name, then human readable city name, and human
// radable country name, none of which are in the continent/capital scheme
// used by the IANA zoneinfo names.
#[derive(Debug, Deserialize)]
struct Place {
    iana_zone: String,
    city_name: String,
    country_name: String,
}

// a struct storing the Place information transformed into absolute and
// relative offsets usable when ordering and displaying times.
#[derive(Debug)]
struct Locality {
    zone: TimeZone,
    is_zulu: bool,
    is_home: bool,
    offset_zulu: i32,  // seconds
    offset_local: i32, // seconds
    city_name: String,
    country_name: String,
    abbreviation: String,
}

impl PartialEq for Locality {
    fn eq(&self, other: &Self) -> bool {
        self.offset_zulu == other.offset_zulu
    }
}

impl Eq for Locality {}

impl PartialOrd for Locality {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.offset_zulu.partial_cmp(&other.offset_zulu)
    }
}

impl Ord for Locality {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.offset_zulu.cmp(&other.offset_zulu)
    }
}

// return the path to the tzlist configuration file in the XDG_CONFIG_DIR.
fn find_tzlist_file() -> Result<PathBuf, std::io::Error> {
    let mut path =
        dirs::config_dir().expect("XDG_CONFIG_DIR not set and default fallback not working either");
    path.push("slashtime");
    path.push("tzlist");

    if path.exists() {
        Ok(path)
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "tzlist file not found",
        ))
    }
}

// parse a file containing three tab separated columns: first with a IANA zone
// info name, second city name, third country name. Ignore lines beginning
// with # as comments
fn tzinfo_parser(filename: &Path) -> Result<Vec<Place>, csv::Error> {
    let file = File::open(filename)?;
    let mut rdr = ReaderBuilder::new()
        .delimiter(b'\t')
        .comment(Some(b'#'))
        .has_headers(false)
        .from_reader(file);

    let mut places = Vec::new();
    for result in rdr.deserialize() {
        let place: Place = result?;
        places.push(place);
    }
    Ok(places)
}

fn format_line(location: &Locality, when: &DateTime) -> String {
    format!(
        "{:22.22}  {}  {}  {}  {}",
        format_locality(location),
        format_time(when),
        format_date(when),
        format_abbreviation(location),
        format_offset(location)
    )
}

fn format_locality(location: &Locality) -> String {
    format!("{}, {}", &location.city_name, &location.country_name)
}

fn format_time(when: &DateTime) -> String {
    format!("{:02}:{:02}", when.hour(), when.minute())
}

fn format_date(when: &DateTime) -> String {
    format!(
        "{}, {:2} {} {}",
        format_day(when.week_day()),
        when.month_day(),
        format_month(when.month()),
        when.year()
    )
}

fn format_day(day: u8) -> String {
    match day {
        0 => "Sun",
        1 => "Mon",
        2 => "Tue",
        3 => "Wed",
        4 => "Thu",
        5 => "Fri",
        6 => "Sat",
        _ => "???",
    }
    .to_string()
}

fn format_month(mon: u8) -> String {
    match mon {
        1 => "Jan",
        2 => "Feb",
        3 => "Mar",
        4 => "Apr",
        5 => "May",
        6 => "Jun",
        7 => "Jul",
        8 => "Aug",
        9 => "Sep",
        10 => "Oct",
        11 => "Nov",
        12 => "Dec",
        _ => "???",
    }
    .to_string()
}

// handle some known exceptions. Singapore's zoneinfo file, for example,
// returns a code of "+08" which is annoying seeing as how there is a widely
// used abbreviation for Singapre Time.
fn refine_zone_abbreviation(iana_zone: &str, code: &str) -> String {
    match iana_zone {
        "America/Sao_Paulo" => "BRT",
        "Asia/Singapore" => "SGT",
        "Asia/Dubai" => "GST",
        "Asia/Tashkent" => "UZT",
        _ => code,
    }
    .to_string()
}

fn format_abbreviation(location: &Locality) -> String {
    format!("{:>4}", &location.abbreviation)
}

fn format_offset(location: &Locality) -> String {
    let offset_minutes = location.offset_local / 60;
    let hours = offset_minutes / 60;
    let halves = if offset_minutes % 60 == 0 { ' ' } else { '½' };

    if offset_minutes == 0 {
        format!("  0 ")
    } else if offset_minutes == -30 {
        // handle the annoying case of a half hour behind needing to show -ve
        format!(" -0½")
    } else {
        format!("{:+3}{:1}", hours, halves)
    }
}

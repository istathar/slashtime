use csv::ReaderBuilder;
use serde::Deserialize;
use std::fs::File;
use tz::DateTime;
use tz::TimeZone;

fn main() -> Result<(), tz::TzError> {
    let lima = tz::TimeZone::local()?;
    let local_offset = lima.find_current_local_time_type()?.ut_offset();

    let now = tz::UtcDateTime::now()?;

    // Ingest the user's tzinfo file.

    let places = tzinfo_parser().unwrap();

    // We now set about converting into Localities. First add an entry for
    // UTC, then convert the user supplied places.

    let mut locations = Vec::with_capacity(places.len() + 1);

    locations.push(Locality {
        zone: tz::TimeZone::utc(),
        offset_zulu: 0,
        offset_local: 0 - local_offset,
        city_name: "Zulu".to_string(),
        country_name: "Universal Time".to_string(),
    });

    // Now add an entry for each of the places present in the tzinfo file.

    for place in places {
        let tz = tz::TimeZone::from_posix_tz(&place.iana_zone)?;
        let offset = tz.find_current_local_time_type()?.ut_offset();
        locations.push(Locality {
            zone: tz,
            offset_zulu: offset,
            offset_local: offset - local_offset,
            city_name: place.city_name,
            country_name: place.country_name,
        });
    }

    // Order the locations by offset.

    locations.sort();

    // Output the formatted locality, time, date, and offest for each location.

    for location in locations {
        let there = now.project(location.zone.as_ref())?;
        println!("{}", format_line(&location, &there));
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

#[derive(Debug)]
struct Locality {
    zone: TimeZone,
    offset_zulu: i32,  // seconds
    offset_local: i32, // seconds
    city_name: String,
    country_name: String,
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

// parse a file containing three tab separated columns: first with a IANA zone
// info name, second city name, third country name. Ignore lines beginning
// with # as comments
fn tzinfo_parser() -> Result<Vec<Place>, csv::Error> {
    let file = File::open("/home/andrew/.config/slashtime/tzlist")?;
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
        "{:22.22}  {}  {}  {}",
        format_locality(location),
        format_time(when),
        format_date(when),
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

use csv::ReaderBuilder;
use serde::Deserialize;
use std::fs::File;
use std::io::Read;
use tz::DateTime;
use tz::TimeZone;
use tz::TimeZoneRef;

fn main() -> Result<(), tz::TzError> {
    let lima = TimeZone::local()?;
    let utc = TimeZone::utc();
    let now = tz::UtcDateTime::now()?;
    println!("UTC now:\t{}", now);

    let places = tzinfo_parser().unwrap();

    for place in places {
        let tz = tz::TimeZone::from_posix_tz(&place.iana_zone)?;

        let there = now.project(tz.as_ref())?;
        println!("{}", format_line(&place, &there));
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

fn format_line(place: &Place, when: &DateTime) -> String {
    format!(
        "{:22.22}  {}  {}",
        format_place(place),
        format_time(when),
        format_date(when)
    )
}

fn format_place(place: &Place) -> String {
    format!("{}, {}", &place.city_name, &place.country_name)
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

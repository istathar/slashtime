use csv::ReaderBuilder;
use serde::Deserialize;
use std::fs::File;
use std::io::Read;
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
        println!("{}, {}\t{}", &place.city_name, &place.country_name, there);
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

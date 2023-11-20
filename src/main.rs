use csv::ReaderBuilder;
use serde::Deserialize;
use std::fs::File;
use std::io::Read;
use tz::TimeZone;
use tz::TimeZoneRef;
use tz::TzError;

fn main() -> Result<(), TzError> {
    let lima = TimeZone::local()?;
    let xray = tz::TimeZone::from_posix_tz("Australia/Sydney")?;
    let utc = TimeZoneRef::utc();
    let now = tz::UtcDateTime::now()?;
    println!("UTC now:\t{}", now);

    let now2 = tz::DateTime::now(lima.as_ref())?;
    println!("Local now:\t{}", now2);

    let east: TimeZone = tz::TimeZone::from_posix_tz("US/Eastern")?;

    let now3 = tz::DateTime::now(east.as_ref())?;
    println!("Eastern time:\t{}", now3);

    // now we make a historical date

    let tranquility = tz::DateTime::find(1969, 07, 20, 16, 17, 00, 0, east.as_ref())?
        .earliest()
        .unwrap();

    println!("The Eagle has landed:\n\t\t{}", tranquility);

    let tranquility2 = tranquility.project(utc)?;
    println!("The Eagle has landed:\n\t\t{}", tranquility2);

    let tranquility3 = tranquility.project(xray.as_ref())?;
    println!("The Eagle has landed:\n\t\t{}", tranquility3);

    // Get the current time zone as a string.
    let tz_str = iana_time_zone::get_timezone().unwrap();
    println!("The current time zone is: {}", tz_str);

    println!("And now {:?}", lima.find_current_local_time_type()?);

    let places = tzinfo_parser().unwrap();

    for place in places {
        println!("{:?}", place);

        let tz = tz::TimeZone::from_posix_tz(&place.iana_zone)?;
        let now = tz::DateTime::now(tz.as_ref())?;

        println!("{}", now);
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
        .has_headers(false)
        .comment(Some(b'#'))
        .from_reader(file);

    let mut places = Vec::new();
    for result in rdr.deserialize() {
        let place: Place = result?;
        places.push(place);
    }
    Ok(places)
}

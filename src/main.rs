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

    tzinfo_parser();

    Ok(())
}

// a struct for the IANA zone name, then human readable city name, and human
// radable country name, none of which are in the continent/capital scheme
// used by the IANA zoneinfo names.
struct Place {
    iana_zone: String,
    city_name: String,
    country_name: String,
}

// parse a file containing three tab separated columns: first with a IANA zone
// info name, second city name, third country name. Ignore lines beginning
// with # as comments
fn tzinfo_parser() -> Vec<Place> {
    let mut places = Vec::new();
    let mut file =
        File::open("/home/andrew/.config/slashtime/tzlist").expect("Couldn't open tzlist file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Couldn't read file");
    for line in contents.lines() {
        if line.starts_with("#") {
            continue;
        }
        let fields: Vec<&str> = line.split('\t').collect();
        let place = Place {
            iana_zone: fields[0].to_string(),
            city_name: fields[1].to_string(),
            country_name: fields[2].to_string(),
        };
        places.push(place);
    }
    places
}

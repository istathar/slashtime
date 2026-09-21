use super::Locality;
use csv::ReaderBuilder;
use serde::Deserialize;
use std::fs::File;
use std::path::{Path, PathBuf};

// a struct for the IANA zone name, then human readable city name, and human
// radable country name, none of which are in the continent/capital scheme
// used by the IANA zoneinfo names. It just represents the CSV-ish data in the
// tzlist file.
#[derive(Debug, Deserialize)]
struct Place {
    iana_zone: String,
    city_name: String,
    country_name: String,
}

// Load the user's tzlist into Localities. The places argument is the file to
// read, overriding the usual one in the config directory; pass None to use
// that. The home argument is the IANA name of a zone to mark as home, which
// matters only when it differs from the machine's own time zone; pass None to
// leave it unmarked.
pub fn load_tzlist(places: Option<&Path>, home: Option<&str>) -> Result<Vec<Locality>, tz::Error> {
    let now = tz::UtcDateTime::now()?;
    let lima = tz::TimeZone::local()?;

    // Ingest the user's tzinfo file.

    let path = match places {
        Some(path) => path.to_path_buf(),
        None => default_tzlist_file(),
    };

    if !path.exists() {
        return Err(tz::Error::Io(Box::new(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("tzlist file {} not found", path.display()),
        ))));
    }

    let places = tzinfo_parser(&path).unwrap();

    // We now set about converting into Localities. First add an entry for
    // UTC, then convert the user supplied places.

    let mut locations = Vec::with_capacity(places.len() + 1);

    locations.push(Locality {
        zone: tz::TimeZone::utc(),
        iana_zone: "UTC".to_string(),
        city_name: "Zulu".to_string(),
        country_name: "Universal Time".to_string(),
        is_zulu: true,
        is_local: false,
        is_home: false,
    });

    // Now add an entry for each of the places present in the tzinfo file.

    for place in places {
        let zone = tz::TimeZone::from_posix_tz(&place.iana_zone)?;
        let local = zone == lima;
        let away = home == Some(place.iana_zone.as_str());

        locations.push(Locality {
            zone,
            is_zulu: false,
            is_local: local,
            is_home: away,
            iana_zone: place.iana_zone,
            city_name: place.city_name,
            country_name: place.country_name,
        });
    }

    // Order the locations by their offset from UTC as at now.

    locations.sort_by_key(|location| location.offset(&now).unwrap_or(0));

    Ok(locations)
}

// the path to the tzlist configuration file in the user's config directory,
// which on Linux is $XDG_CONFIG_HOME or ~/.config.
fn default_tzlist_file() -> PathBuf {
    let mut path = dirs::config_dir().expect("unable to determine the user's config directory");
    path.push("slashtime");
    path.push("tzlist");
    path
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

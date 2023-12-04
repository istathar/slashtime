use super::{refine_zone_abbreviation, Locality, Place};
use csv::ReaderBuilder;
use std::fs::File;
use std::path::{Path, PathBuf};

pub fn load_tzlist() -> Result<Vec<Locality>, tz::TzError> {
    let lima = tz::TimeZone::local()?;
    let local_offset = lima.find_current_local_time_type()?.ut_offset();

    // Ingest the user's tzinfo file.

    let path = find_tzlist_file()?;
    let places = tzinfo_parser(&path).unwrap();

    // We now set about converting into Localities. First add an ent}ry for
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

    Ok(locations)
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

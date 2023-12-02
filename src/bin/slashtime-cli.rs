use slashtime::{Locality, find_tzlist_file, tzinfo_parser, format_line, refine_zone_abbreviation};
use crossterm::{
    queue,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::{Clear, ClearType},
};

fn main() -> Result<(), tz::TzError> {
    let lima = tz::TimeZone::local()?;
    let local_offset = lima.find_current_local_time_type()?.ut_offset();

    let now = tz::UtcDateTime::now()?;

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

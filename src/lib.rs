use tz::DateTime;
use tz::TimeZone;
use tz::TzError;
use tz::UtcDateTime;

pub mod loading;

// a Place from the tzlist, resolved against the system zoneinfo database.
// Anything that varies with time - the offset from UTC, the zone
// abbreviation - is looked up on demand rather than stored here, so that a
// program left running for months stays correct across a daylight savings
// transition.
#[derive(Clone, Debug)]
pub struct Locality {
    pub zone: TimeZone,
    pub iana_zone: String,
    pub city_name: String,
    pub country_name: String,
    pub is_zulu: bool,
    pub is_local: bool,
    pub is_home: bool,
}

// how reachable someone is at their local hour. White for business hours,
// gray for the hours when it is still civilized to call someone, black for
// the night.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Band {
    Work,
    Civil,
    Night,
}

// the original reckoned the day in half hours from midnight, which is enough
// resolution for the zones that are offset by thirty minutes.
fn halves_since_midnight(there: &DateTime) -> u8 {
    there.hour() * 2 + if there.minute() >= 30 { 1 } else { 0 }
}

impl Locality {
    // the offset from UTC in effect here at the given moment, in seconds.
    pub fn offset(&self, when: &UtcDateTime) -> Result<i32, TzError> {
        let local = self.zone.find_local_time_type(when.unix_time())?;

        Ok(local.ut_offset())
    }

    // which band the local hour falls into: 09:00 to 17:00 is the working
    // day, 07:00 to 23:00 is still civilized, the rest is night.
    pub fn band(&self, when: &UtcDateTime) -> Result<Band, TzError> {
        let there = when.project(self.zone.as_ref())?;

        Ok(match halves_since_midnight(&there) {
            18..=33 => Band::Work,
            14..=45 => Band::Civil,
            _ => Band::Night,
        })
    }

    // where this location sorts: by local time of day, but rotated so that
    // the small hours fall to the bottom. Black at the bottom means someone
    // hard core may still be up working; black at the top means they are
    // asleep.
    pub fn sort_key(&self, when: &UtcDateTime) -> Result<u8, TzError> {
        let there = when.project(self.zone.as_ref())?;
        let halves = halves_since_midnight(&there);

        Ok(if halves < 3 { halves + 48 } else { halves })
    }

    // the zone abbreviation in effect here at the given moment; "AEST" in
    // winter becomes "AEDT" once daylight savings starts.
    pub fn abbreviation(&self, when: &UtcDateTime) -> Result<String, TzError> {
        let local = self.zone.find_local_time_type(when.unix_time())?;

        Ok(refine_zone_abbreviation(
            &self.iana_zone,
            local.time_zone_designation(),
        ))
    }
}

// Output a single line with all the relevant information. The target is the
// location being represented, and pivot is the location its offset is
// measured from. That is usually wherever you are now, but the whole point of
// the program is that you can measure from somewhere else instead.
pub fn format_line(
    target: &Locality,
    pivot: &Locality,
    when: &UtcDateTime,
) -> Result<String, TzError> {
    let there = when.project(target.zone.as_ref())?;
    let offset_seconds = target.offset(when)? - pivot.offset(when)?;

    Ok(format!(
        "{:22.22}  {}  {}  {}  {}",
        format_locality(target),
        format_time(&there),
        format_date(&there),
        format_abbreviation(&target.abbreviation(when)?),
        format_offset(offset_seconds)
    ))
}

fn format_locality(location: &Locality) -> String {
    format!("{}, {}", &location.city_name, &location.country_name)
}

pub fn format_time(when: &DateTime) -> String {
    format!("{:02}:{:02}", when.hour(), when.minute())
}

// two digit year, as both the perl and the java original used; the century is
// not in doubt and the column is narrow.
pub fn format_date(when: &DateTime) -> String {
    format!(
        "{}, {:2} {} {:02}",
        format_day(when.week_day()),
        when.month_day(),
        format_month(when.month()),
        when.year() % 100
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
// used abbreviation for Singapre Time. UTC carries no designation at all.
fn refine_zone_abbreviation(iana_zone: &str, code: &str) -> String {
    match iana_zone {
        "UTC" => "UTC",
        "America/Sao_Paulo" => "BRT",
        "Asia/Singapore" => "SGT",
        "Asia/Dubai" => "GST",
        "Asia/Tashkent" => "UZT",
        _ => code,
    }
    .to_string()
}

fn format_abbreviation(code: &str) -> String {
    format!("{:>4}", code)
}

pub fn format_offset(offset_seconds: i32) -> String {
    let offset_minutes = offset_seconds / 60;
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

// which location offsets are measured from by default: wherever the system
// clock says we are. Falls back to Zulu if the tzlist happens not to contain
// the local zone.
pub fn find_local(locations: &[Locality]) -> Option<usize> {
    locations
        .iter()
        .position(|location| location.is_local)
        .or_else(|| locations.iter().position(|location| location.is_zulu))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn locality(iana_zone: &str) -> Locality {
        Locality {
            zone: TimeZone::from_posix_tz(iana_zone).unwrap(),
            iana_zone: iana_zone.to_string(),
            city_name: "Somewhere".to_string(),
            country_name: "Someplace".to_string(),
            is_zulu: false,
            is_local: false,
            is_home: false,
        }
    }

    fn at(hour: u8, minute: u8) -> UtcDateTime {
        UtcDateTime::new(2026, 7, 1, hour, minute, 0, 0).unwrap()
    }

    // the shading follows each location's own local hour, not the pivot's.
    #[test]
    fn bands_follow_the_local_hour() {
        let zulu = locality("UTC");

        assert_eq!(zulu.band(&at(6, 59)).unwrap(), Band::Night);
        assert_eq!(zulu.band(&at(7, 0)).unwrap(), Band::Civil);
        assert_eq!(zulu.band(&at(8, 59)).unwrap(), Band::Civil);
        assert_eq!(zulu.band(&at(9, 0)).unwrap(), Band::Work);
        assert_eq!(zulu.band(&at(16, 59)).unwrap(), Band::Work);
        assert_eq!(zulu.band(&at(17, 0)).unwrap(), Band::Civil);
        assert_eq!(zulu.band(&at(22, 59)).unwrap(), Band::Civil);
        assert_eq!(zulu.band(&at(23, 0)).unwrap(), Band::Night);
    }

    // the day turns over at 01:30, which is when hackers go to bed, so the
    // small hours sort to the bottom of the list rather than the top.
    #[test]
    fn the_day_turns_over_at_half_past_one() {
        let zulu = locality("UTC");

        assert_eq!(zulu.sort_key(&at(1, 30)).unwrap(), 3);
        assert_eq!(zulu.sort_key(&at(23, 30)).unwrap(), 47);
        assert_eq!(zulu.sort_key(&at(0, 0)).unwrap(), 48);
        assert_eq!(zulu.sort_key(&at(1, 0)).unwrap(), 50);
    }

    // the offset and the abbreviation both have to track daylight savings,
    // otherwise a program left running across the transition goes an hour
    // wrong and stays that way.
    #[test]
    fn daylight_savings_is_not_cached() {
        let sydney = locality("Australia/Sydney");

        let winter = UtcDateTime::new(2026, 7, 1, 0, 0, 0, 0).unwrap();
        let summer = UtcDateTime::new(2027, 1, 1, 0, 0, 0, 0).unwrap();

        assert_eq!(sydney.offset(&winter).unwrap(), 10 * 3600);
        assert_eq!(sydney.abbreviation(&winter).unwrap(), "AEST");

        assert_eq!(sydney.offset(&summer).unwrap(), 11 * 3600);
        assert_eq!(sydney.abbreviation(&summer).unwrap(), "AEDT");
    }

    // the offset shown is relative to the pivot, not to UTC. Sydney in winter
    // is fourteen hours ahead of Toronto, which is the whole point.
    #[test]
    fn offset_is_measured_from_the_pivot() {
        let sydney = locality("Australia/Sydney");
        let toronto = locality("America/Toronto");

        let when = UtcDateTime::new(2026, 7, 1, 0, 0, 0, 0).unwrap();
        let offset = sydney.offset(&when).unwrap() - toronto.offset(&when).unwrap();

        assert_eq!(format_offset(offset), "+14 ");
    }

    // UTC has no zone designation of its own in the database.
    #[test]
    fn zulu_is_abbreviated() {
        let mut zulu = locality("UTC");
        zulu.is_zulu = true;

        let when = UtcDateTime::new(2026, 7, 1, 0, 0, 0, 0).unwrap();

        assert_eq!(zulu.abbreviation(&when).unwrap(), "UTC");
    }
}

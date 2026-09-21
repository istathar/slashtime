use tz::DateTime;
use tz::TimeZone;
use tz::TimeZoneRef;
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

    // The moment at which the clock here reads the given local time. Daylight
    // savings makes that ambiguous twice a year: where the reading happens
    // twice take the earlier, and where it is skipped over entirely the answer
    // is the moment the clock jumps to, which is the nearest real time to the
    // one asked for.
    pub fn instant(
        &self,
        year: i32,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
    ) -> Result<Option<UtcDateTime>, TzError> {
        let found = DateTime::find(year, month, day, hour, minute, 0, 0, self.zone.as_ref())?;

        match found.earliest() {
            Some(there) => Ok(Some(UtcDateTime::from_timespec(there.unix_time(), 0)?)),
            None => Ok(None),
        }
    }

    // the zone abbreviation in effect here at the given moment; "AEST" in
    // winter becomes "AEDT" once daylight savings starts.
    pub fn abbreviation(&self, when: &UtcDateTime) -> Result<&str, TzError> {
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
        format_date_full(&there),
        format_abbreviation(target.abbreviation(when)?),
        format_offset(offset_seconds)
    ))
}

fn format_locality(location: &Locality) -> String {
    format!("{}, {}", &location.city_name, &location.country_name)
}

pub fn format_time(when: &DateTime) -> String {
    format!("{:02}:{:02}", when.hour(), when.minute())
}

// The date on its own. The day of the week is not here because the list sets
// it beside the time on the line above, which leaves the column narrow enough
// to carry the year in full.
pub fn format_date(when: &DateTime) -> String {
    format!(
        "{:2} {} {}",
        when.month_day(),
        format_month(when.month()),
        when.year()
    )
}

// with the weekday back in front, for a line that is going to be read
// somewhere else, away from anything that would supply it
fn format_date_full(when: &DateTime) -> String {
    format!("{}, {}", format_day(when.week_day()), format_date(when))
}

pub fn format_day(day: u8) -> &'static str {
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
fn refine_zone_abbreviation<'a>(iana_zone: &str, code: &'a str) -> &'a str {
    match iana_zone {
        "UTC" => "UTC",
        "America/Sao_Paulo" => "BRT",
        "Asia/Singapore" => "SGT",
        "Asia/Dubai" => "GST",
        "Asia/Tashkent" => "UZT",
        _ => code,
    }
}

fn format_abbreviation(code: &str) -> String {
    format!("{:>4}", code)
}

// The hours of an offset, and separately whether there is a half hour on the
// end. Keeping them apart lets a caller reserve a fixed slot for the ½ so that
// the half hour zones do not shove the units column sideways.
pub fn format_offset_parts(offset_seconds: i32) -> (String, bool) {
    let offset_minutes = offset_seconds / 60;
    let hours = offset_minutes / 60;
    let half = offset_minutes % 60 != 0;

    let text = if offset_minutes == 0 {
        "0".to_string()
    } else if offset_minutes == -30 {
        // handle the annoying case of a half hour behind needing to show -ve
        "-0".to_string()
    } else {
        format!("{:+}", hours)
    };

    (text, half)
}

// In a terminal the trailing space and the ½ are the same width, so padding to
// a fixed four columns is enough to hold the units steady.
pub fn format_offset(offset_seconds: i32) -> String {
    let (text, half) = format_offset_parts(offset_seconds);

    format!("{:>3}{}", text, if half { '½' } else { ' ' })
}

// which day of the week a date falls on, Sunday counting as zero, so that a
// calendar knows how far into the first row to begin
pub fn week_day(year: i32, month: u8, day: u8) -> Result<u8, TzError> {
    let noon = DateTime::find(year, month, day, 12, 0, 0, 0, TimeZoneRef::utc())?;

    Ok(noon.earliest().map_or(0, |when| when.week_day()))
}

// how many days a month has, which a calendar needs in order to lay itself out
pub fn days_in_month(year: i32, month: u8) -> u8 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) => 29,
        _ => 28,
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

    // setting a wall clock time somewhere gives back the moment it happens,
    // which is what lets the rest of the list be worked out from it.
    #[test]
    fn a_local_time_resolves_to_an_instant() {
        let sydney = locality("Australia/Sydney");
        let toronto = locality("America/Toronto");

        // 09:00 in Toronto in July is 23:00 in Sydney the same day
        let when = toronto.instant(2026, 7, 1, 9, 0).unwrap().unwrap();
        let there = when.project(sydney.zone.as_ref()).unwrap();

        assert_eq!(there.hour(), 23);
        assert_eq!(there.month_day(), 1);
    }

    // a local time inside the hour daylight savings skips never happens, so
    // the answer is the moment the clock jumps to
    #[test]
    fn a_skipped_hour_lands_on_the_transition() {
        let sydney = locality("Australia/Sydney");

        // clocks go forward at 02:00 on the first Sunday in October
        let when = sydney.instant(2026, 10, 4, 2, 30).unwrap().unwrap();
        let there = when.project(sydney.zone.as_ref()).unwrap();

        assert_eq!((there.hour(), there.minute()), (3, 0));
    }

    // and a local time in the hour that happens twice takes the earlier one
    #[test]
    fn a_repeated_hour_takes_the_earlier() {
        let sydney = locality("Australia/Sydney");

        // clocks go back at 03:00 on the first Sunday in April
        let when = sydney.instant(2026, 4, 5, 2, 30).unwrap().unwrap();

        assert_eq!(sydney.offset(&when).unwrap(), 11 * 3600);
    }

    #[test]
    fn dates_fall_on_the_right_weekday() {
        assert_eq!(week_day(2026, 9, 17).unwrap(), 4); // a Thursday
        assert_eq!(week_day(2026, 9, 20).unwrap(), 0); // a Sunday
        assert_eq!(week_day(2000, 1, 1).unwrap(), 6); // a Saturday
    }

    // the screen keeps the weekday on the line above; a line that leaves the
    // program carries it, and the century, itself
    #[test]
    fn a_line_that_leaves_carries_its_century() {
        let when = UtcDateTime::new(2026, 7, 1, 12, 0, 0, 0)
            .unwrap()
            .project(TimeZoneRef::utc())
            .unwrap();

        assert_eq!(format_date(&when), " 1 Jul 2026");
        assert_eq!(format_date_full(&when), "Wed,  1 Jul 2026");
    }

    #[test]
    fn months_have_the_right_number_of_days() {
        assert_eq!(days_in_month(2026, 1), 31);
        assert_eq!(days_in_month(2026, 2), 28);
        assert_eq!(days_in_month(2024, 2), 29);
        assert_eq!(days_in_month(2000, 2), 29);
        assert_eq!(days_in_month(1900, 2), 28);
        assert_eq!(days_in_month(2026, 9), 30);
    }

    // the ½ occupies a slot that is reserved whether or not it is there, so
    // that the units column stays put down the whole list.
    #[test]
    fn half_hours_do_not_disturb_the_units_column() {
        assert_eq!(format_offset(0), "  0 ");
        assert_eq!(format_offset(2 * 3600), " +2 ");
        assert_eq!(format_offset(-14 * 3600), "-14 ");
        assert_eq!(format_offset(-30 * 60), " -0½");
        assert_eq!(format_offset(-4 * 3600 - 30 * 60), " -4½");
        assert_eq!(format_offset(9 * 3600 + 30 * 60), " +9½");

        // every rendering is the same width, ½ or no ½
        for seconds in [0, 3600, -3600, -30 * 60, 12 * 3600 + 1800, -20 * 3600] {
            assert_eq!(format_offset(seconds).chars().count(), 4);
        }
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

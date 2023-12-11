use tz::DateTime;
use tz::TimeZone;

pub mod loading;

// a struct storing the Place information transformed into absolute and
// relative offsets usable when ordering and displaying times.
#[derive(Clone, Debug)]
pub struct Locality {
    pub zone: TimeZone,
    pub is_zulu: bool,
    pub is_home: bool,
    pub offset_zulu: i32,  // seconds
    pub offset_local: i32, // seconds
    pub city_name: String,
    pub country_name: String,
    pub abbreviation: String,
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

// Output a single line with all the relevant information. The target is the
// location that is being represented, and from is the location this is
// relative to (usually the home aka where you are now).
pub fn format_line(target: &Locality, from: &Locality, when: &DateTime) -> String {
    // FIXME not current time, but at timestamp
    let offset_target = (target.zone)
        .find_current_local_time_type()
        .unwrap()
        .ut_offset();

    let offset_home = (from.zone)
        .find_current_local_time_type()
        .unwrap()
        .ut_offset();

    let offset_seconds = offset_target - offset_home;

    format!(
        "{:22.22}  {}  {}  {}  {}",
        format_locality(target),
        format_time(when),
        format_date(when),
        format_abbreviation(target),
        format_offset(offset_seconds)
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

// handle some known exceptions. Singapore's zoneinfo file, for example,
// returns a code of "+08" which is annoying seeing as how there is a widely
// used abbreviation for Singapre Time.
pub fn refine_zone_abbreviation(iana_zone: &str, code: &str) -> String {
    match iana_zone {
        "America/Sao_Paulo" => "BRT",
        "Asia/Singapore" => "SGT",
        "Asia/Dubai" => "GST",
        "Asia/Tashkent" => "UZT",
        _ => code,
    }
    .to_string()
}

fn format_abbreviation(location: &Locality) -> String {
    format!("{:>4}", &location.abbreviation)
}

fn format_offset(offset_seconds: i32) -> String {
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

pub fn find_home(locations: &[Locality]) -> Option<&Locality> {
    for location in locations {
        if location.is_home {
            return Some(location);
        }
    }
    None
}

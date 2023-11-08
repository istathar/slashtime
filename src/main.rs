use chrono::{DateTime, Datelike, Local, NaiveDate, TimeZone, Timelike, Utc};
use chrono_tz::Tz;

fn main() {
    let local_now = Local::now();
    println!("Local now: {}", local_now.to_rfc3339());

    let tz: Tz = "US/Eastern".parse().unwrap();

    let dt = tz.with_ymd_and_hms(1969, 07, 20, 16, 17, 00).unwrap();
    println!("The Eagle has landed: {}", dt.to_rfc3339());

    let dt2 = dt.with_timezone(&Utc);
    println!("The Eagle has landed: {}", dt2.to_rfc3339());

    // Get the current time zone as a string.
    let tz_str = iana_time_zone::get_timezone().unwrap();
    println!("The current time zone is: {}", tz_str);

    let dt3 = dt.with_timezone(&Local);
    println!("The Eagle has landed: {}", dt3.to_rfc3339());
}

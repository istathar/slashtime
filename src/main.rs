use chrono::{DateTime, Datelike, Local, NaiveDate, TimeZone, Timelike, Utc};

fn main() {
    let local_now = Local::now();
    println!("Local now: {}", local_now);
}

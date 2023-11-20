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

    Ok(())
}

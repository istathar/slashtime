use crossterm::{
    queue,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::{Clear, ClearType},
};
use slashtime::{find_local, format_line};

fn main() -> Result<(), tz::TzError> {
    let now = tz::UtcDateTime::now()?;

    let locations = slashtime::loading::load_tzlist(None)?;

    // Offsets are measured from the location in the machine's own time zone,
    // unless a zone is named on the command line, in which case they are
    // measured from there instead.

    let pivot = match std::env::args().nth(1) {
        Some(name) => locations
            .iter()
            .position(|location| location.iana_zone == name)
            .unwrap_or_else(|| {
                eprintln!("Zone \"{}\" is not present in your tzlist", name);
                std::process::exit(1);
            }),
        None => find_local(&locations).unwrap(),
    };
    let pivot = &locations[pivot];

    // Output the formatted locality, time, date, and offest for each location.

    let mut out = std::io::stdout();

    for location in &locations {
        let line = format_line(location, pivot, &now)?;

        if location.is_zulu {
            // using the macro
            queue!(
                out,
                SetForegroundColor(Color::DarkGreen),
                Print(line),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n"),
            )?;
        } else if location.is_local {
            queue!(
                out,
                SetForegroundColor(Color::DarkCyan),
                Print(line),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n")
            )?;
        } else {
            queue!(
                out,
                Print(line),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n"),
            )?;
        }
    }

    Ok(())
}

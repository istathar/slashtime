use crossterm::{
    queue,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::{Clear, ClearType},
};
use slashtime::{find_home, format_line};

fn main() -> Result<(), tz::TzError> {
    let now = tz::UtcDateTime::now()?;

    let locations = slashtime::loading::load_tzlist()?;
    let home = find_home(&locations).unwrap();

    // Output the formatted locality, time, date, and offest for each location.

    let mut out = std::io::stdout();

    for location in &locations {
        let there = now.project(
            location
                .zone
                .as_ref(),
        )?;
        if location.is_zulu {
            // using the macro
            queue!(
                out,
                SetForegroundColor(Color::DarkGreen),
                Print(format_line(&location, &home, &there)),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n"),
            )?;
        } else if location.is_home {
            queue!(
                out,
                SetForegroundColor(Color::DarkCyan),
                Print(format_line(&location, &home, &there)),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n")
            )?;
        } else {
            queue!(
                out,
                Print(format_line(&location, &home, &there)),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n"),
            )?;
        }
    }

    Ok(())
}
